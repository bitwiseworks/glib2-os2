/* gspawn-os2.c - Process launching on OS/2 (based on gspawn.c)
 *
 *  Copyright 2000 Red Hat, Inc.
 *  g_execvpe implementation based on GNU libc execvp:
 *   Copyright 1991, 92, 95, 96, 97, 98, 99 Free Software Foundation, Inc.
 *
 * GLib is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * GLib is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with GLib; see the file COPYING.LIB.  If not, write
 * to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include "config.h"

#include <sys/time.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <stdlib.h>   /* for fdwalk */
#include <dirent.h>
#include <process.h>
#include <sys/socket.h>

#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif /* HAVE_SYS_SELECT_H */

#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif /* HAVE_SYS_RESOURCE_H */

#include "glib.h"
#include "glibintl.h"

static gint g_execute (const gchar  *file,
                       gchar **argv,
                       gchar **envp,
                       gboolean search_path);

static gboolean make_pipe            (gint                  p[2],
                                      GError              **error);

static gboolean fork_exec_with_pipes (gboolean              intermediate_child,
                                      const gchar          *working_directory,
                                      gchar               **argv,
                                      gchar               **envp,
                                      gboolean              close_descriptors,
                                      gboolean              search_path,
                                      gboolean              stdout_to_null,
                                      gboolean              stderr_to_null,
                                      gboolean              child_inherits_stdin,
                                      gboolean              file_and_argv_zero,
                                      GSpawnChildSetupFunc  child_setup,
                                      gpointer              user_data,
                                      GPid                 *child_pid,
                                      gint                 *standard_input,
                                      gint                 *standard_output,
                                      gint                 *standard_error,
                                      GError              **error);

static gboolean fork_exec_with_fds (gboolean              intermediate_child,
                                      const gchar          *working_directory,
                                      gchar               **argv,
                                      gchar               **envp,
                                      gboolean              close_descriptors,
                                      gboolean              search_path,
                                      gboolean              stdout_to_null,
                                      gboolean              stderr_to_null,
                                      gboolean              child_inherits_stdin,
                                      gboolean              file_and_argv_zero,
                                      GSpawnChildSetupFunc  child_setup,
                                      gpointer              user_data,
                                      GPid                 *child_pid,
                                      gint                 *child_close_fds,
                                      gint                  stdin_fd,
                                      gint                  stdout_fd,
                                      gint                  stderr_fd,
                                      GError              **error);

GQuark
g_spawn_error_quark (void)
{
  return g_quark_from_static_string ("g-exec-error-quark");
}

G_DEFINE_QUARK (g-spawn-exit-error-quark, g_spawn_exit_error)

/**
 * g_spawn_async:
 * @working_directory: child's current working directory, or %NULL to inherit parent's
 * @argv: child's argument vector
 * @envp: child's environment, or %NULL to inherit parent's
 * @flags: flags from #GSpawnFlags
 * @child_setup: function to run in the child just before exec()
 * @user_data: user data for @child_setup
 * @child_pid: return location for child process reference, or %NULL
 * @error: return location for error
 * 
 * See g_spawn_async_with_pipes() for a full description; this function
 * simply calls the g_spawn_async_with_pipes() without any pipes.
 *
 * You should call g_spawn_close_pid() on the returned child process
 * reference when you don't need it any more.
 * 
 * <note><para>
 * If you are writing a GTK+ application, and the program you 
 * are spawning is a graphical application, too, then you may
 * want to use gdk_spawn_on_screen() instead to ensure that 
 * the spawned program opens its windows on the right screen.
 * </para></note>
 *
 * <note><para> Note that the returned @child_pid on Windows is a
 * handle to the child process and not its identifier. Process handles
 * and process identifiers are different concepts on Windows.
 * </para></note>
 *
 * Return value: %TRUE on success, %FALSE if error is set
 **/
gboolean
g_spawn_async (const gchar          *working_directory,
               gchar               **argv,
               gchar               **envp,
               GSpawnFlags           flags,
               GSpawnChildSetupFunc  child_setup,
               gpointer              user_data,
               GPid                 *child_pid,
               GError              **error)
{
  g_return_val_if_fail (argv != NULL, FALSE);
  
  return g_spawn_async_with_pipes (working_directory,
                                   argv, envp,
                                   flags,
                                   child_setup,
                                   user_data,
                                   child_pid,
                                   NULL, NULL, NULL,
                                   error);
}

/* Avoids a danger in threaded situations (calling close()
 * on a file descriptor twice, and another thread has
 * re-opened it since the first close)
 */
static gint
close_and_invalidate (gint *fd)
{
  gint ret;

  if (*fd < 0)
    return -1;
  else
    {
      ret = close (*fd);
      *fd = -1;
    }

  return ret;
}

/* Some versions of OS X define READ_OK in public headers */
#undef READ_OK

typedef enum
{
  READ_FAILED = 0, /* FALSE */
  READ_OK,
  READ_EOF
} ReadResult;

static ReadResult
read_data (GString *str,
           gint     fd,
           GError **error)
{
  gssize bytes;        
  gchar buf[4096];    

 again:
  
  bytes = read (fd, buf, 4096);

  if (bytes == 0)
    return READ_EOF;
  else if (bytes > 0)
    {
      g_string_append_len (str, buf, bytes);
      return READ_OK;
    }
  else if (bytes < 0 && errno == EINTR)
    goto again;
  else if (bytes < 0)
    {
      int errsv = errno;

      g_set_error (error,
                   G_SPAWN_ERROR,
                   G_SPAWN_ERROR_READ,
                   _("Failed to read data from child process (%s)"),
                   g_strerror (errsv));
      
      return READ_FAILED;
    }
  else
    return READ_OK;
}

/**
 * g_spawn_sync:
 * @working_directory: child's current working directory, or %NULL to inherit parent's
 * @argv: child's argument vector
 * @envp: child's environment, or %NULL to inherit parent's
 * @flags: flags from #GSpawnFlags 
 * @child_setup: function to run in the child just before exec()
 * @user_data: user data for @child_setup
 * @standard_output: return location for child output, or %NULL
 * @standard_error: return location for child error messages, or %NULL
 * @exit_status: return location for child exit status, as returned by waitpid(), or %NULL
 * @error: return location for error, or %NULL
 *
 * Executes a child synchronously (waits for the child to exit before returning).
 * All output from the child is stored in @standard_output and @standard_error,
 * if those parameters are non-%NULL. Note that you must set the  
 * %G_SPAWN_STDOUT_TO_DEV_NULL and %G_SPAWN_STDERR_TO_DEV_NULL flags when
 * passing %NULL for @standard_output and @standard_error.
 * If @exit_status is non-%NULL, the exit status of the child is stored
 * there as it would be returned by waitpid(); standard UNIX macros such 
 * as WIFEXITED() and WEXITSTATUS() must be used to evaluate the exit status.
 * Note that this function call waitpid() even if @exit_status is %NULL, and
 * does not accept the %G_SPAWN_DO_NOT_REAP_CHILD flag.
 * If an error occurs, no data is returned in @standard_output, 
 * @standard_error, or @exit_status. 
 *
 * This function calls g_spawn_async_with_pipes() internally; see that
 * function for full details on the other parameters and details on
 * how these functions work on Windows.
 * 
 * Return value: %TRUE on success, %FALSE if an error was set.
 **/
gboolean
g_spawn_sync (const gchar          *working_directory,
              gchar               **argv,
              gchar               **envp,
              GSpawnFlags           flags,
              GSpawnChildSetupFunc  child_setup,
              gpointer              user_data,
              gchar               **standard_output,
              gchar               **standard_error,
              gint                 *exit_status,
              GError              **error)     
{
  gint outpipe = -1;
  gint errpipe = -1;
  GPid pid;
  fd_set fds;
  gint ret;
  GString *outstr = NULL;
  GString *errstr = NULL;
  gboolean failed;
  gint status;
  
  g_return_val_if_fail (argv != NULL, FALSE);
  g_return_val_if_fail (!(flags & G_SPAWN_DO_NOT_REAP_CHILD), FALSE);
  g_return_val_if_fail (standard_output == NULL ||
                        !(flags & G_SPAWN_STDOUT_TO_DEV_NULL), FALSE);
  g_return_val_if_fail (standard_error == NULL ||
                        !(flags & G_SPAWN_STDERR_TO_DEV_NULL), FALSE);
  
  /* Just to ensure segfaults if callers try to use
   * these when an error is reported.
   */
  if (standard_output)
    *standard_output = NULL;

  if (standard_error)
    *standard_error = NULL;
  
  if (!fork_exec_with_pipes (FALSE,
                             working_directory,
                             argv,
                             envp,
                             !(flags & G_SPAWN_LEAVE_DESCRIPTORS_OPEN),
                             (flags & G_SPAWN_SEARCH_PATH) != 0,
                             (flags & G_SPAWN_STDOUT_TO_DEV_NULL) != 0,
                             (flags & G_SPAWN_STDERR_TO_DEV_NULL) != 0,
                             (flags & G_SPAWN_CHILD_INHERITS_STDIN) != 0,
                             (flags & G_SPAWN_FILE_AND_ARGV_ZERO) != 0,
                             child_setup,
                             user_data,
                             &pid,
                             NULL,
                             standard_output ? &outpipe : NULL,
                             standard_error ? &errpipe : NULL,
                             error))
    return FALSE;

  /* Read data from child. */
  
  failed = FALSE;

  if (outpipe >= 0)
    {
      outstr = g_string_new (NULL);
    }
      
  if (errpipe >= 0)
    {
      errstr = g_string_new (NULL);
    }

  /* Read data until we get EOF on both pipes. */
  while (!failed &&
         (outpipe >= 0 ||
          errpipe >= 0))
    {
      ret = 0;
          
      FD_ZERO (&fds);
      if (outpipe >= 0)
        FD_SET (outpipe, &fds);
      if (errpipe >= 0)
        FD_SET (errpipe, &fds);
          
      ret = select (MAX (outpipe, errpipe) + 1,
                    &fds,
                    NULL, NULL,
                    NULL /* no timeout */);

      if (ret < 0 && errno != EINTR)
        {
          int errsv = errno;

          failed = TRUE;

          g_set_error (error,
                       G_SPAWN_ERROR,
                       G_SPAWN_ERROR_READ,
                       _("Unexpected error in select() reading data from a child process (%s)"),
                       g_strerror (errsv));
              
          break;
        }

      if (outpipe >= 0 && FD_ISSET (outpipe, &fds))
        {
          switch (read_data (outstr, outpipe, error))
            {
            case READ_FAILED:
              failed = TRUE;
              break;
            case READ_EOF:
              close_and_invalidate (&outpipe);
              outpipe = -1;
              break;
            default:
              break;
            }

          if (failed)
            break;
        }

      if (errpipe >= 0 && FD_ISSET (errpipe, &fds))
        {
          switch (read_data (errstr, errpipe, error))
            {
            case READ_FAILED:
              failed = TRUE;
              break;
            case READ_EOF:
              close_and_invalidate (&errpipe);
              errpipe = -1;
              break;
            default:
              break;
            }

          if (failed)
            break;
        }
    }

  /* These should only be open still if we had an error.  */
  
  if (outpipe >= 0)
    close_and_invalidate (&outpipe);
  if (errpipe >= 0)
    close_and_invalidate (&errpipe);
  
  /* Wait for child to exit, even if we have
   * an error pending.
   */
 again:
      
  ret = waitpid (pid, &status, 0);

  if (ret < 0)
    {
      if (errno == EINTR)
        goto again;
      else if (errno == ECHILD)
        {
          if (exit_status)
            {
              g_warning ("In call to g_spawn_sync(), exit status of a child process was requested but SIGCHLD action was set to SIG_IGN and ECHILD was received by waitpid(), so exit status can't be returned. This is a bug in the program calling g_spawn_sync(); either don't request the exit status, or don't set the SIGCHLD action.");
            }
          else
            {
              /* We don't need the exit status. */
            }
        }
      else
        {
          if (!failed) /* avoid error pileups */
            {
              int errsv = errno;

              failed = TRUE;
                  
              g_set_error (error,
                           G_SPAWN_ERROR,
                           G_SPAWN_ERROR_READ,
                           _("Unexpected error in waitpid() (%s)"),
                           g_strerror (errsv));
            }
        }
    }
  
  if (failed)
    {
      if (outstr)
        g_string_free (outstr, TRUE);
      if (errstr)
        g_string_free (errstr, TRUE);

      return FALSE;
    }
  else
    {
      if (exit_status)
        *exit_status = status;
      
      if (standard_output)        
        *standard_output = g_string_free (outstr, FALSE);

      if (standard_error)
        *standard_error = g_string_free (errstr, FALSE);

      return TRUE;
    }
}

static void
reap_child (GPid pid)
{
 wait_failed:
  if (waitpid (pid, NULL, 0) < 0)
    {
       if (errno == EINTR)
         goto wait_failed;
       else if (errno == ECHILD)
         ; /* do nothing, child already reaped */
       else
         g_warning ("waitpid() should not fail in "
                    "'reap_child'");
    }
}

static void
reap_child_thread (void *pid)
{
  reap_child ((GPid) pid);
}

/**
 * g_spawn_async_with_pipes:
 * @working_directory: child's current working directory, or %NULL to inherit parent's, in the GLib file name encoding
 * @argv: child's argument vector, in the GLib file name encoding
 * @envp: child's environment, or %NULL to inherit parent's, in the GLib file name encoding
 * @flags: flags from #GSpawnFlags
 * @child_setup: function to run in the child just before exec()
 * @user_data: user data for @child_setup
 * @child_pid: return location for child process ID, or %NULL
 * @standard_input: return location for file descriptor to write to child's stdin, or %NULL
 * @standard_output: return location for file descriptor to read child's stdout, or %NULL
 * @standard_error: return location for file descriptor to read child's stderr, or %NULL
 * @error: return location for error
 *
 * Executes a child program asynchronously (your program will not
 * block waiting for the child to exit). The child program is
 * specified by the only argument that must be provided, @argv. @argv
 * should be a %NULL-terminated array of strings, to be passed as the
 * argument vector for the child. The first string in @argv is of
 * course the name of the program to execute. By default, the name of
 * the program must be a full path; the <envar>PATH</envar> shell variable 
 * will only be searched if you pass the %G_SPAWN_SEARCH_PATH flag.
 *
 * On Windows, note that all the string or string vector arguments to
 * this function and the other g_spawn*() functions are in UTF-8, the
 * GLib file name encoding. Unicode characters that are not part of
 * the system codepage passed in these arguments will be correctly
 * available in the spawned program only if it uses wide character API
 * to retrieve its command line. For C programs built with Microsoft's
 * tools it is enough to make the program have a wmain() instead of
 * main(). wmain() has a wide character argument vector as parameter.
 *
 * At least currently, mingw doesn't support wmain(), so if you use
 * mingw to develop the spawned program, it will have to call the
 * undocumented function __wgetmainargs() to get the wide character
 * argument vector and environment. See gspawn-win32-helper.c in the
 * GLib sources or init.c in the mingw runtime sources for a prototype
 * for that function. Alternatively, you can retrieve the Win32 system
 * level wide character command line passed to the spawned program
 * using the GetCommandLineW() function.
 *
 * On Windows the low-level child process creation API
 * <function>CreateProcess()</function> doesn't use argument vectors,
 * but a command line. The C runtime library's
 * <function>spawn*()</function> family of functions (which
 * g_spawn_async_with_pipes() eventually calls) paste the argument
 * vector elements together into a command line, and the C runtime startup code
 * does a corresponding reconstruction of an argument vector from the
 * command line, to be passed to main(). Complications arise when you have
 * argument vector elements that contain spaces of double quotes. The
 * <function>spawn*()</function> functions don't do any quoting or
 * escaping, but on the other hand the startup code does do unquoting
 * and unescaping in order to enable receiving arguments with embedded
 * spaces or double quotes. To work around this asymmetry,
 * g_spawn_async_with_pipes() will do quoting and escaping on argument
 * vector elements that need it before calling the C runtime
 * spawn() function.
 *
 * The returned @child_pid on Windows is a handle to the child
 * process, not its identifier. Process handles and process
 * identifiers are different concepts on Windows.
 *
 * @envp is a %NULL-terminated array of strings, where each string
 * has the form <literal>KEY=VALUE</literal>. This will become
 * the child's environment. If @envp is %NULL, the child inherits its
 * parent's environment.
 *
 * @flags should be the bitwise OR of any flags you want to affect the
 * function's behaviour. The %G_SPAWN_DO_NOT_REAP_CHILD means that 
 * the child will not automatically be reaped; you must use a
 * #GChildWatch source to be notified about the death of the child 
 * process. Eventually you must call g_spawn_close_pid() on the
 * @child_pid, in order to free resources which may be associated
 * with the child process. (On Unix, using a #GChildWatch source is
 * equivalent to calling waitpid() or handling the %SIGCHLD signal 
 * manually. On Windows, calling g_spawn_close_pid() is equivalent
 * to calling CloseHandle() on the process handle returned in 
 * @child_pid).
 *
 * %G_SPAWN_LEAVE_DESCRIPTORS_OPEN means that the parent's open file
 * descriptors will be inherited by the child; otherwise all
 * descriptors except stdin/stdout/stderr will be closed before
 * calling exec() in the child. %G_SPAWN_SEARCH_PATH 
 * means that <literal>argv[0]</literal> need not be an absolute path, it
 * will be looked for in the user's <envar>PATH</envar>. 
 * %G_SPAWN_STDOUT_TO_DEV_NULL means that the child's standard output will 
 * be discarded, instead of going to the same location as the parent's 
 * standard output. If you use this flag, @standard_output must be %NULL.
 * %G_SPAWN_STDERR_TO_DEV_NULL means that the child's standard error
 * will be discarded, instead of going to the same location as the parent's
 * standard error. If you use this flag, @standard_error must be %NULL.
 * %G_SPAWN_CHILD_INHERITS_STDIN means that the child will inherit the parent's
 * standard input (by default, the child's standard input is attached to
 * /dev/null). If you use this flag, @standard_input must be %NULL.
 * %G_SPAWN_FILE_AND_ARGV_ZERO means that the first element of @argv is
 * the file to execute, while the remaining elements are the
 * actual argument vector to pass to the file. Normally
 * g_spawn_async_with_pipes() uses @argv[0] as the file to execute, and
 * passes all of @argv to the child.
 *
 * @child_setup and @user_data are a function and user data. On POSIX
 * platforms, the function is called in the child after GLib has
 * performed all the setup it plans to perform (including creating
 * pipes, closing file descriptors, etc.) but before calling
 * exec(). That is, @child_setup is called just
 * before calling exec() in the child. Obviously
 * actions taken in this function will only affect the child, not the
 * parent.
 *
 * On Windows, there is no separate fork() and exec()
 * functionality. Child processes are created and run with a single
 * API call, CreateProcess(). There is no sensible thing @child_setup
 * could be used for on Windows so it is ignored and not called.
 *
 * If non-%NULL, @child_pid will on Unix be filled with the child's
 * process ID. You can use the process ID to send signals to the
 * child, or to use g_child_watch_add() (or waitpid()) if you specified the
 * %G_SPAWN_DO_NOT_REAP_CHILD flag. On Windows, @child_pid will be
 * filled with a handle to the child process only if you specified the
 * %G_SPAWN_DO_NOT_REAP_CHILD flag. You can then access the child
 * process using the Win32 API, for example wait for its termination
 * with the <function>WaitFor*()</function> functions, or examine its
 * exit code with GetExitCodeProcess(). You should close the handle 
 * with CloseHandle() or g_spawn_close_pid() when you no longer need it.
 *
 * If non-%NULL, the @standard_input, @standard_output, @standard_error
 * locations will be filled with file descriptors for writing to the child's
 * standard input or reading from its standard output or standard error.
 * The caller of g_spawn_async_with_pipes() must close these file descriptors
 * when they are no longer in use. If these parameters are %NULL, the corresponding
 * pipe won't be created.
 *
 * If @standard_input is NULL, the child's standard input is attached to 
 * /dev/null unless %G_SPAWN_CHILD_INHERITS_STDIN is set.
 *
 * If @standard_error is NULL, the child's standard error goes to the same 
 * location as the parent's standard error unless %G_SPAWN_STDERR_TO_DEV_NULL 
 * is set.
 *
 * If @standard_output is NULL, the child's standard output goes to the same 
 * location as the parent's standard output unless %G_SPAWN_STDOUT_TO_DEV_NULL 
 * is set.
 *
 * @error can be %NULL to ignore errors, or non-%NULL to report errors.
 * If an error is set, the function returns %FALSE. Errors
 * are reported even if they occur in the child (for example if the
 * executable in <literal>argv[0]</literal> is not found). Typically
 * the <literal>message</literal> field of returned errors should be displayed
 * to users. Possible errors are those from the #G_SPAWN_ERROR domain.
 *
 * If an error occurs, @child_pid, @standard_input, @standard_output,
 * and @standard_error will not be filled with valid values.
 *
 * If @child_pid is not %NULL and an error does not occur then the returned
 * process reference must be closed using g_spawn_close_pid().
 *
 * <note><para>
 * If you are writing a GTK+ application, and the program you 
 * are spawning is a graphical application, too, then you may
 * want to use gdk_spawn_on_screen_with_pipes() instead to ensure that 
 * the spawned program opens its windows on the right screen.
 * </para></note>
 * 
 * Return value: %TRUE on success, %FALSE if an error was set
 **/
gboolean
g_spawn_async_with_pipes (const gchar          *working_directory,
                          gchar               **argv,
                          gchar               **envp,
                          GSpawnFlags           flags,
                          GSpawnChildSetupFunc  child_setup,
                          gpointer              user_data,
                          GPid                 *child_pid,
                          gint                 *standard_input,
                          gint                 *standard_output,
                          gint                 *standard_error,
                          GError              **error)
{
  gboolean result;
  GPid pid = -1;

  g_return_val_if_fail (argv != NULL, FALSE);
  g_return_val_if_fail (standard_output == NULL ||
                        !(flags & G_SPAWN_STDOUT_TO_DEV_NULL), FALSE);
  g_return_val_if_fail (standard_error == NULL ||
                        !(flags & G_SPAWN_STDERR_TO_DEV_NULL), FALSE);
  /* can't inherit stdin if we have an input pipe. */
  g_return_val_if_fail (standard_input == NULL ||
                        !(flags & G_SPAWN_CHILD_INHERITS_STDIN), FALSE);

  result = fork_exec_with_pipes (!(flags & G_SPAWN_DO_NOT_REAP_CHILD),
                               working_directory,
                               argv,
                               envp,
                               !(flags & G_SPAWN_LEAVE_DESCRIPTORS_OPEN),
                               (flags & G_SPAWN_SEARCH_PATH) != 0,
                               (flags & G_SPAWN_STDOUT_TO_DEV_NULL) != 0,
                               (flags & G_SPAWN_STDERR_TO_DEV_NULL) != 0,
                               (flags & G_SPAWN_CHILD_INHERITS_STDIN) != 0,
                               (flags & G_SPAWN_FILE_AND_ARGV_ZERO) != 0,
                               child_setup,
                               user_data,
                               &pid,
                               standard_input,
                               standard_output,
                               standard_error,
                               error);
  if (child_pid)
    *child_pid = pid;

  /* start a thread with waitpid() so the return code does not fill up memory
   * unless G_SPAWN_DO_NOT_REAP_CHILD is given in which case it's a
   * responsibility of the caller.
   */
  if (result && pid > 0 && !(flags & G_SPAWN_DO_NOT_REAP_CHILD))
    _beginthread (reap_child_thread, NULL, 0x2000, (void *) pid);

  return result;
}

/**
 * g_spawn_async_with_fds:
 * @working_directory: (type filename) (nullable): child's current working directory, or %NULL to inherit parent's, in the GLib file name encoding
 * @argv: (array zero-terminated=1): child's argument vector, in the GLib file name encoding
 * @envp: (array zero-terminated=1) (nullable): child's environment, or %NULL to inherit parent's, in the GLib file name encoding
 * @flags: flags from #GSpawnFlags
 * @child_setup: (scope async) (nullable): function to run in the child just before exec()
 * @user_data: (closure): user data for @child_setup
 * @child_pid: (out) (optional): return location for child process ID, or %NULL
 * @stdin_fd: file descriptor to use for child's stdin, or -1
 * @stdout_fd: file descriptor to use for child's stdout, or -1
 * @stderr_fd: file descriptor to use for child's stderr, or -1
 * @error: return location for error
 *
 * Identical to g_spawn_async_with_pipes() but instead of
 * creating pipes for the stdin/stdout/stderr, you can pass existing
 * file descriptors into this function through the @stdin_fd,
 * @stdout_fd and @stderr_fd parameters. The following @flags
 * also have their behaviour slightly tweaked as a result:
 *
 * %G_SPAWN_STDOUT_TO_DEV_NULL means that the child's standard output
 * will be discarded, instead of going to the same location as the parent's
 * standard output. If you use this flag, @standard_output must be -1.
 * %G_SPAWN_STDERR_TO_DEV_NULL means that the child's standard error
 * will be discarded, instead of going to the same location as the parent's
 * standard error. If you use this flag, @standard_error must be -1.
 * %G_SPAWN_CHILD_INHERITS_STDIN means that the child will inherit the parent's
 * standard input (by default, the child's standard input is attached to
 * /dev/null). If you use this flag, @standard_input must be -1.
 *
 * It is valid to pass the same fd in multiple parameters (e.g. you can pass
 * a single fd for both stdout and stderr).
 *
 * Returns: %TRUE on success, %FALSE if an error was set
 *
 * Since: 2.58
 */
gboolean
g_spawn_async_with_fds (const gchar          *working_directory,
                        gchar               **argv,
                        gchar               **envp,
                        GSpawnFlags           flags,
                        GSpawnChildSetupFunc  child_setup,
                        gpointer              user_data,
                        GPid                 *child_pid,
                        gint                  stdin_fd,
                        gint                  stdout_fd,
                        gint                  stderr_fd,
                        GError              **error)
{
  gboolean result;
  GPid pid = -1;

  g_return_val_if_fail (argv != NULL, FALSE);
  g_return_val_if_fail (stdout_fd < 0 ||
                        !(flags & G_SPAWN_STDOUT_TO_DEV_NULL), FALSE);
  g_return_val_if_fail (stderr_fd < 0 ||
                        !(flags & G_SPAWN_STDERR_TO_DEV_NULL), FALSE);
  /* can't inherit stdin if we have an input pipe. */
  g_return_val_if_fail (stdin_fd < 0 ||
                        !(flags & G_SPAWN_CHILD_INHERITS_STDIN), FALSE);

  result = fork_exec_with_fds (!(flags & G_SPAWN_DO_NOT_REAP_CHILD),
                               working_directory,
                               argv,
                               envp,
                               !(flags & G_SPAWN_LEAVE_DESCRIPTORS_OPEN),
                               (flags & G_SPAWN_SEARCH_PATH) != 0,
                               (flags & G_SPAWN_STDOUT_TO_DEV_NULL) != 0,
                               (flags & G_SPAWN_STDERR_TO_DEV_NULL) != 0,
                               (flags & G_SPAWN_CHILD_INHERITS_STDIN) != 0,
                               (flags & G_SPAWN_FILE_AND_ARGV_ZERO) != 0,
                               child_setup,
                               user_data,
                               &pid,
                               NULL,
                               stdin_fd,
                               stdout_fd,
                               stderr_fd,
                               error);
  if (child_pid)
    *child_pid = pid;

  /* start a thread with waitpid() so the return code does not fill up memory
   * unless G_SPAWN_DO_NOT_REAP_CHILD is given in which case it's a
   * responsibility of the caller.
   */
  if (result && pid > 0 && !(flags & G_SPAWN_DO_NOT_REAP_CHILD))
    _beginthread (reap_child_thread, NULL, 0x2000, (void *) pid);

  return result;
}

/**
 * g_spawn_command_line_sync:
 * @command_line: a command line 
 * @standard_output: return location for child output
 * @standard_error: return location for child errors
 * @exit_status: return location for child exit status, as returned by waitpid()
 * @error: return location for errors
 *
 * A simple version of g_spawn_sync() with little-used parameters
 * removed, taking a command line instead of an argument vector.  See
 * g_spawn_sync() for full details. @command_line will be parsed by
 * g_shell_parse_argv(). Unlike g_spawn_sync(), the %G_SPAWN_SEARCH_PATH flag
 * is enabled. Note that %G_SPAWN_SEARCH_PATH can have security
 * implications, so consider using g_spawn_sync() directly if
 * appropriate. Possible errors are those from g_spawn_sync() and those
 * from g_shell_parse_argv().
 *
 * If @exit_status is non-%NULL, the exit status of the child is stored there as
 * it would be returned by waitpid(); standard UNIX macros such as WIFEXITED()
 * and WEXITSTATUS() must be used to evaluate the exit status.
 * 
 * On Windows, please note the implications of g_shell_parse_argv()
 * parsing @command_line. Parsing is done according to Unix shell rules, not 
 * Windows command interpreter rules.
 * Space is a separator, and backslashes are
 * special. Thus you cannot simply pass a @command_line containing
 * canonical Windows paths, like "c:\\program files\\app\\app.exe", as
 * the backslashes will be eaten, and the space will act as a
 * separator. You need to enclose such paths with single quotes, like
 * "'c:\\program files\\app\\app.exe' 'e:\\folder\\argument.txt'".
 *
 * Return value: %TRUE on success, %FALSE if an error was set
 **/
gboolean
g_spawn_command_line_sync (const gchar  *command_line,
                           gchar       **standard_output,
                           gchar       **standard_error,
                           gint         *exit_status,
                           GError      **error)
{
  gboolean retval;
  gchar **argv = NULL;

  g_return_val_if_fail (command_line != NULL, FALSE);
  
  if (!g_shell_parse_argv (command_line,
                           NULL, &argv,
                           error))
    return FALSE;
  
  retval = g_spawn_sync (NULL,
                         argv,
                         NULL,
                         G_SPAWN_SEARCH_PATH,
                         NULL,
                         NULL,
                         standard_output,
                         standard_error,
                         exit_status,
                         error);
  g_strfreev (argv);

  return retval;
}

/**
 * g_spawn_command_line_async:
 * @command_line: a command line
 * @error: return location for errors
 * 
 * A simple version of g_spawn_async() that parses a command line with
 * g_shell_parse_argv() and passes it to g_spawn_async(). Runs a
 * command line in the background. Unlike g_spawn_async(), the
 * %G_SPAWN_SEARCH_PATH flag is enabled, other flags are not. Note
 * that %G_SPAWN_SEARCH_PATH can have security implications, so
 * consider using g_spawn_async() directly if appropriate. Possible
 * errors are those from g_shell_parse_argv() and g_spawn_async().
 * 
 * The same concerns on Windows apply as for g_spawn_command_line_sync().
 *
 * Return value: %TRUE on success, %FALSE if error is set.
 **/
gboolean
g_spawn_command_line_async (const gchar *command_line,
                            GError     **error)
{
  gboolean retval;
  gchar **argv = NULL;

  g_return_val_if_fail (command_line != NULL, FALSE);

  if (!g_shell_parse_argv (command_line,
                           NULL, &argv,
                           error))
    return FALSE;
  
  retval = g_spawn_async (NULL,
                          argv,
                          NULL,
                          G_SPAWN_SEARCH_PATH,
                          NULL,
                          NULL,
                          NULL,
                          error);
  g_strfreev (argv);

  return retval;
}

static int
set_cloexec (void *data, gint fd)
{
  if (fd >= GPOINTER_TO_INT (data))
    fcntl (fd, F_SETFD, FD_CLOEXEC);

  return 0;
}

#ifndef HAVE_FDWALK
static int
fdwalk (int (*cb)(void *data, int fd), void *data)
{
  gint open_max;
  gint fd;
  gint res = 0;
  
  open_max = 256; /*sysconf (_SC_OPEN_MAX)*/

  for (fd = 0; fd < open_max; fd++)
      if ((res = cb (data, fd)) != 0)
          break;

  return res;
}
#endif

static gint
sane_dup2 (gint fd1, gint fd2)
{
  gint ret;

 retry:
  ret = dup2 (fd1, fd2);
  if (ret < 0 && errno == EINTR)
    goto retry;

  return ret;
}

static gint
do_exec (gint                  stdin_fd,
         gint                  stdout_fd,
         gint                  stderr_fd,
         const gchar          *working_directory,
         gchar               **argv,
         gchar               **envp,
         gboolean              close_descriptors,
         gboolean              search_path,
         gboolean              stdout_to_null,
         gboolean              stderr_to_null,
         gboolean              child_inherits_stdin,
         gboolean              file_and_argv_zero,
         GSpawnChildSetupFunc  child_setup,
         gpointer              user_data,
         GError                **error)
{
  gint stdin_sav = -1;
  gint stdout_sav = -1;
  gint stderr_sav = -1;
  gint pid = -1;
  char wd[1024];

  /* Save current working directory
   */
  if (working_directory && *working_directory)
    _getcwd2 (wd,sizeof(wd));

  if (working_directory && *working_directory && _chdir2 (working_directory) < 0)
    g_set_error (error,
                 G_SPAWN_ERROR,
                 G_SPAWN_ERROR_CHDIR,
                 _("Failed to change to directory '%s' (%s)"),
                 working_directory,
                 g_strerror (errno));

  /* Close all file descriptors but stdin stdout and stderr as
   * soon as we exec. Note that this includes
   * child_err_report_fd, which keeps the parent from blocking
   * forever on the other end of that pipe.
   */
  if (close_descriptors)
    {
      fdwalk (set_cloexec, GINT_TO_POINTER(3));
    }

  /* Redirect pipes as required
   * Save the handles before closing them!!!!
   */

  if (stdin_fd >= 0)
    {
      stdin_sav = dup (0);
      /* dup2 can't actually fail here I don't think */
          
      if (sane_dup2 (stdin_fd, 0) < 0)
        g_set_error (error,
                     G_SPAWN_ERROR,
                     G_SPAWN_ERROR_FAILED,
                     _("Failed to redirect output or input of child process (%s)"),
                     g_strerror (errno));

      /* ignore this if it doesn't work */
      close_and_invalidate (&stdin_fd);
    }
  else if (!child_inherits_stdin)
    {
      /* Keep process from blocking on a read of stdin */
      gint read_null = open ("NUL", O_RDONLY);
      stdin_sav = dup (0);
      sane_dup2 (read_null, 0);
      close_and_invalidate (&read_null);
    }

  if (stdout_fd >= 0)
    {
      stdout_sav = dup (1);
      /* dup2 can't actually fail here I don't think */
          
      if (sane_dup2 (stdout_fd, 1) < 0)
        g_set_error (error,
                     G_SPAWN_ERROR,
                     G_SPAWN_ERROR_FAILED,
                     _("Failed to redirect output or input of child process (%s)"),
                     g_strerror (errno));

      /* ignore this if it doesn't work */
      close_and_invalidate (&stdout_fd);
    }
  else if (stdout_to_null)
    {
      gint write_null = open ("NUL", O_WRONLY);
      stdout_sav = dup (1);
      sane_dup2 (write_null, 1);
      close_and_invalidate (&write_null);
    }

  if (stderr_fd >= 0)
    {
      stderr_sav = dup (2);
      /* dup2 can't actually fail here I don't think */
          
      if (sane_dup2 (stderr_fd, 2) < 0)
        g_set_error (error,
                     G_SPAWN_ERROR,
                     G_SPAWN_ERROR_FAILED,
                     _("Failed to redirect output or input of child process (%s)"),
                     g_strerror (errno));

      /* ignore this if it doesn't work */
      close_and_invalidate (&stderr_fd);
    }
  else if (stderr_to_null)
    {
      gint write_null = open ("NUL", O_WRONLY);
      stderr_sav = dup (2);
      sane_dup2 (write_null, 2);
      close_and_invalidate (&write_null);
    }
  
  /* Call user function just before we exec */
  if (child_setup)
    {
      (* child_setup) (user_data);
    }

  pid = g_execute (argv[0],
             file_and_argv_zero ? argv + 1 : argv,
             envp, search_path);

  /* Restore working directory */
  if (working_directory && *working_directory)
    _chdir2 (wd);

  /* Restore the saved handles
   */
  if (stdin_sav > -1)
    {
      sane_dup2 (stdin_sav, 0);
      close (stdin_sav);
    }

  if (stdout_sav > -1)
    {
      sane_dup2 (stdout_sav, 1);
      close (stdout_sav);
    }

  if (stderr_sav > -1)
    {
      sane_dup2 (stderr_sav, 2);
      close (stderr_sav);
    }

  return pid;
}

static gboolean
fork_exec_with_pipes (gboolean              intermediate_child,
                      const gchar          *working_directory,
                      gchar               **argv,
                      gchar               **envp,
                      gboolean              close_descriptors,
                      gboolean              search_path,
                      gboolean              stdout_to_null,
                      gboolean              stderr_to_null,
                      gboolean              child_inherits_stdin,
                      gboolean              file_and_argv_zero,
                      GSpawnChildSetupFunc  child_setup,
                      gpointer              user_data,
                      GPid                 *child_pid,
                      gint                 *standard_input,
                      gint                 *standard_output,
                      gint                 *standard_error,
                      GError              **error)     
{
  GPid pid = -1;
  gint stdin_pipe[2] = { -1, -1 };
  gint stdout_pipe[2] = { -1, -1 };
  gint stderr_pipe[2] = { -1, -1 };

  if (standard_input && !make_pipe (stdin_pipe, error))
    goto cleanup_and_fail;
  
  if (standard_output && !make_pipe (stdout_pipe, error))
    goto cleanup_and_fail;

  if (standard_error && !make_pipe (stderr_pipe, error))
    goto cleanup_and_fail;

  /* Close the parent's end of the pipes;
   * not needed in the close_descriptors case,
   * though
   */
  set_cloexec (GINT_TO_POINTER(0), stdin_pipe[1]);
  set_cloexec (GINT_TO_POINTER(0), stdout_pipe[0]);
  set_cloexec (GINT_TO_POINTER(0), stderr_pipe[0]);

  /* Spawn the child
   */
  pid = do_exec (stdin_pipe[0],
                stdout_pipe[1],
                stderr_pipe[1],
                working_directory,
                argv,
                envp,
                close_descriptors,
                search_path,
                stdout_to_null,
                stderr_to_null,
                child_inherits_stdin,
                file_and_argv_zero,
                child_setup,
                user_data,
                error);

  if (pid <= 0)
      goto cleanup_and_fail;

  /* Success against all odds! return the information */

  /* Close the uncared-about ends of the pipes */
  close_and_invalidate (&stdin_pipe[0]);
  close_and_invalidate (&stdout_pipe[1]);
  close_and_invalidate (&stderr_pipe[1]);

  if (child_pid)
    *child_pid = pid;

  if (standard_input)
    *standard_input = stdin_pipe[1];
  if (standard_output)
    *standard_output = stdout_pipe[0];
  if (standard_error)
    *standard_error = stderr_pipe[0];

  return TRUE;

 cleanup_and_fail:

  close_and_invalidate (&stdin_pipe[0]);
  close_and_invalidate (&stdin_pipe[1]);
  close_and_invalidate (&stdout_pipe[0]);
  close_and_invalidate (&stdout_pipe[1]);
  close_and_invalidate (&stderr_pipe[0]);
  close_and_invalidate (&stderr_pipe[1]);

  return FALSE;
}

// this needs a review later on
static gboolean
fork_exec_with_fds (gboolean              intermediate_child,
                      const gchar          *working_directory,
                      gchar               **argv,
                      gchar               **envp,
                      gboolean              close_descriptors,
                      gboolean              search_path,
                      gboolean              stdout_to_null,
                      gboolean              stderr_to_null,
                      gboolean              child_inherits_stdin,
                      gboolean              file_and_argv_zero,
                      GSpawnChildSetupFunc  child_setup,
                      gpointer              user_data,
                      GPid                 *child_pid,
                      gint                 *child_close_fds,
                      gint                  stdin_fd,
                      gint                  stdout_fd,
                      gint                  stderr_fd,
                      GError              **error)     
{
  GPid pid = -1;

  /* Spawn the child
   */
  pid = do_exec (stdin_fd,
                stdout_fd,
                stderr_fd,
                working_directory,
                argv,
                envp,
                close_descriptors,
                search_path,
                stdout_to_null,
                stderr_to_null,
                child_inherits_stdin,
                file_and_argv_zero,
                child_setup,
                user_data,
                error);

  if (pid <= 0)
      goto cleanup_and_fail;

  if (child_pid)
    *child_pid = pid;

  return TRUE;

 cleanup_and_fail:

  return FALSE;
}

static gboolean
make_pipe (gint     p[2],
           GError **error)
{
  /* Pipes created with pipe() have problems with select(), use
     socketpair() instead.
   */
  if (socketpair (AF_LOCAL, SOCK_STREAM, 0, p) < 0)
    {
      gint errsv = errno;
      g_set_error (error,
                   G_SPAWN_ERROR,
                   G_SPAWN_ERROR_FAILED,
                   _("Failed to create pipe for communicating with child process (%s)"),
                   g_strerror (errsv));
      return FALSE;
    }
  else
    return TRUE;
}

/* Based on execvp from GNU C Library */

static gint
script_execute (const gchar *file,
                gchar      **argv,
                gchar      **envp,
                gboolean     search_path)
{
  /* Count the arguments.  */
  int argc = 0;
  gint pid = -1;

  while (argv[argc])
    ++argc;
  
  /* Construct an argument list for the shell.  */
  {
    gchar **new_argv;

    new_argv = g_new0 (gchar*, argc + 2); /* /bin/sh and NULL */
    
    new_argv[0] = (char *) "sh.exe";
    new_argv[1] = (char *) file;
    while (argc > 0)
      {
	new_argv[argc + 1] = argv[argc];
	--argc;
      }

    /* Execute the shell. */
    if (envp)
      pid = spawnve (P_NOWAIT, new_argv[0], new_argv, envp);
    else
      pid = spawnv (P_NOWAIT, new_argv[0], new_argv);
    
    g_free (new_argv);

    return pid;
  }
}

static gint
g_execute (const gchar *file,
           gchar      **argv,
           gchar      **envp,
           gboolean     search_path)
{
  gint pid;

  if (*file == '\0')
    {
      /* We check the simple case first. */
      errno = ENOENT;
      return -1;
    }

  if (envp)
    pid = spawnve (P_NOWAIT, file, argv, envp);
  else
    pid = spawnv (P_NOWAIT, file, argv);

  if (pid == -1)
    pid = script_execute (file, argv, envp, FALSE);

  return pid;
}

/**
 * g_spawn_close_pid:
 * @pid: The process reference to close
 *
 * On some platforms, notably Windows, the #GPid type represents a resource
 * which must be closed to prevent resource leaking. g_spawn_close_pid()
 * is provided for this purpose. It should be used on all platforms, even
 * though it doesn't do anything under UNIX.
 **/
void
g_spawn_close_pid (GPid pid)
{
}

/**
 * g_spawn_check_exit_status:
 * @exit_status: An exit code as returned from g_spawn_sync()
 * @error: a #GError
 *
 * Set @error if @exit_status indicates the child exited abnormally
 * (e.g. with a nonzero exit code, or via a fatal signal).
 *
 * The g_spawn_sync() and g_child_watch_add() family of APIs return an
 * exit status for subprocesses encoded in a platform-specific way.
 * On Unix, this is guaranteed to be in the same format
 * <literal>waitpid(2)</literal> returns, and on Windows it is
 * guaranteed to be the result of
 * <literal>GetExitCodeProcess()</literal>.  Prior to the introduction
 * of this function in GLib 2.34, interpreting @exit_status required
 * use of platform-specific APIs, which is problematic for software
 * using GLib as a cross-platform layer.
 *
 * Additionally, many programs simply want to determine whether or not
 * the child exited successfully, and either propagate a #GError or
 * print a message to standard error.  In that common case, this
 * function can be used.  Note that the error message in @error will
 * contain human-readable information about the exit status.
 *
 * The <literal>domain</literal> and <literal>code</literal> of @error
 * have special semantics in the case where the process has an "exit
 * code", as opposed to being killed by a signal.  On Unix, this
 * happens if <literal>WIFEXITED</literal> would be true of
 * @exit_status.  On Windows, it is always the case.
 *
 * The special semantics are that the actual exit code will be the
 * code set in @error, and the domain will be %G_SPAWN_EXIT_ERROR.
 * This allows you to differentiate between different exit codes.
 *
 * If the process was terminated by some means other than an exit
 * status, the domain will be %G_SPAWN_ERROR, and the code will be
 * %G_SPAWN_ERROR_FAILED.
 *
 * This function just offers convenience; you can of course also check
 * the available platform via a macro such as %G_OS_UNIX, and use
 * <literal>WIFEXITED()</literal> and <literal>WEXITSTATUS()</literal>
 * on @exit_status directly.  Do not attempt to scan or parse the
 * error message string; it may be translated and/or change in future
 * versions of GLib.
 *
 * Returns: %TRUE if child exited successfully, %FALSE otherwise (and @error will be set)
 * Since: 2.34
 */
gboolean
g_spawn_check_exit_status (gint      exit_status,
			   GError  **error)
{
  gboolean ret = FALSE;

  if (WIFEXITED (exit_status))
    {
      if (WEXITSTATUS (exit_status) != 0)
	{
	  g_set_error (error, G_SPAWN_EXIT_ERROR, WEXITSTATUS (exit_status),
		       _("Child process exited with code %ld"),
		       (long) WEXITSTATUS (exit_status));
	  goto out;
	}
    }
  else if (WIFSIGNALED (exit_status))
    {
      g_set_error (error, G_SPAWN_ERROR, G_SPAWN_ERROR_FAILED,
		   _("Child process killed by signal %ld"),
		   (long) WTERMSIG (exit_status));
      goto out;
    }
  else if (WIFSTOPPED (exit_status))
    {
      g_set_error (error, G_SPAWN_ERROR, G_SPAWN_ERROR_FAILED,
		   _("Child process stopped by signal %ld"),
		   (long) WSTOPSIG (exit_status));
      goto out;
    }
  else
    {
      g_set_error (error, G_SPAWN_ERROR, G_SPAWN_ERROR_FAILED,
		   _("Child process exited abnormally"));
      goto out;
    }

  ret = TRUE;
 out:
  return ret;
}

