;;; vterm/config.el -*- lexical-binding: t; -*-

;; -----------------------------------------------------
;; Usefull emacs lisp spec links:
;; -----------------------------------------------------
;; User indentification:
;;   - https://www.gnu.org/software/emacs/manual/html_node/elisp/User-Identification.html

;; -----------------------------------------------------
;; Descriptions of selected functions used in this file:
;; -----------------------------------------------------
;; file-remote-p
;;
;; Test whether FILE specifies a location on a remote system.
;; A file is considered remote if accessing it is likely to
;; be slower or less reliable than accessing local files.
;; ‘file-remote-p’ never opens a new remote connection.  It can
;; reuse only a connection that is already open.

;; This function returns the remote path prefix, if that prefix is
;; prepended to the path.
;; For example:
;; (file-remote-p "/ssh:piotrek@192.168.0.30:/home/piotrek") returns /ssh:piotrek@192.168.0.30:
;; (file-remote-p "/ssh:pihole-ethernet:/home/piotrek")      returns /ssh:pihole-ethernet:
;; (file-remote-p "/home/piotrek")                           returns nil
;; -----------------------------------------------------
;; file-directory-p
;;
;; This function returns a given path as a directory, by appending
;; '/' at the end of this path.
;; For example:
;; (file-directory-p "test")          returns "test/"
;; (file-directory-p "/home/piotrek") returns "home/piotrek/"
;; -----------------------------------------------------
;; concat
;;
;; Concatenates 2 or more symbols (containing strings) into one
;; -----------------------------------------------------
;; s-starts-with-p
;;
;; Checks whether a given string starts with a given prefix
;; -----------------------------------------------------


;; Original functions overwrites tramp path with a guessed path.
;; However it breaks if remote fqdn/hostname is not resolvale by local machine
;; could also break on port forwarding, multihops,
;; custom protocol such as: docker, vagrant, ...
;; *if* you try to shell-side configure them.
;; Easily testable with vagrant ssh port on localhost.
;; My workflow is to open a tramp dired on / of the remote to get a
;; "foothold" then open vterms from there.
;;
;; The reason why it works only when vterm is run directly from TRAMP dired buffer
;; is that, only in this case, the variable 'default-directory' is set with
;; username and hostname (or ssh config alias) prepended to the path, e.g:
;; /ssh:pihole-ethernet:/home/piotrek/       (with ssh config alias)
;; /ssh:piotrek@192.186.0.30:/home/piotrek/  (with username and hostname)
;; whereas when vterm is opened on a local machine and then connected to
;; remote server with SSH, 'default-directory' is as follows:
;; /home/piotrek
;; so it lacks remote user and hostname prepended to it.
(defun vterm--get-directory (path)
  "[OVERLOADED] Get normalized directory to PATH."
  (when path
    (let (directory)
      (if (string-match "^\\(.*?\\)@\\(.*?\\):\\(.*?\\)$" path)
          (progn
            (let ((user (match-string 1 path))
                  (host (match-string 2 path))
                  (dir (match-string 3 path)))
              (if (and (string-equal user user-login-name)
                       (string-equal host (system-name)))
                  (progn
		    (message "Path on local host")
                    (when (file-directory-p dir)
                      (setq directory (file-name-as-directory dir)))
	            (message (concat "directory: " directory)))
		(progn
		  (message "Path on remote host")
		  (message default-directory)
		  (message (file-remote-p default-directory))
                  (setq directory
			;; Bellow is what i altered
			(file-name-as-directory (concat (file-remote-p default-directory) dir))))))
	    (message (concat "directory: " directory))
	    )
        (when (file-directory-p path)
          (setq directory (file-name-as-directory path))))
      directory)))

;; Injects the payload to the vterm buffer.
(defun my/vterm-load-config ()
  "Pass local configuration files to vterm.
Allows remote vterm to be shell-side configured, without altering
remote config.  Also adds my personal configuration that does not
rely too much on external packages.  Prints a reasuring message
to proove good faith."
  (interactive)
  (let (;; Bellow messages to reassure other users that look at history
        (reasuring-message (format "Configuring shell of user %s to be emacs compatible"
                                   user-full-name))
        (reasuring-notice "This action is shell local, it will not affect other shells")
        ;; Bellow lies my configuration
        (basic-func-script (f-read-text (concat (getenv "HOME")
                                                "/.emacs.d/shells/sources/functions.sh")))
        ;; Bellow lies the vterm shell-side configuration
        ;; Must be sourced last
        (vterm-func-script (f-read-text (concat
                                         (file-name-directory (find-library-name "vterm"))
                                         "/etc/emacs-vterm-bash.sh"))))
    (vterm-insert (format "# START: %s\n" reasuring-message))
    (vterm-insert (format "# %s\n" reasuring-notice))
    ;; Create one single block in .bash_history
    (vterm-insert "{\n")
    (vterm-insert basic-func-script)
    (vterm-insert vterm-func-script)
    (vterm-insert "}\n")
    ;; End the single block in .bash_history
    (vterm-insert (format "# %s\n" reasuring-notice))
    (vterm-insert (format "# STOP: %s\n" reasuring-message))
    )
  )

;; This function is most likely Doom-specific, bit I found it on this
;; reddit thread:
;; https://www.reddit.com/r/emacs/comments/lwkx5u/how_come_i_am_able_to_edit_root_files_by_giving/
;; It is used to detect whether we want to sudo-edit a given file on a local machine or on a remote host.
;; If it detects we want to edit a file on a remote host, it creates a Tramp call with a specific
;; syntax, according to
;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Quick-Start-Guide
;; and
;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Ad_002dhoc-multi_002dhops.html
;;
;; For example
;; (doom--sudo-file-path "/home/piotrek")                        will return /sudo:root@localhost:/home/piotrek
;; (doom--sudo-file-path "/ssh:piotrek@raspberry:/home/piotrek") will return /ssh:piotrek@raspberry|sudo:root@raspberry:/home/piotrek"
(defun doom--sudo-file-path (file)
  (let ((host (or (file-remote-p file 'host) "localhost")))
    (concat "/" (when (file-remote-p file)
                  (concat (file-remote-p file 'method) ":"
                          (if-let (user (file-remote-p file 'user))
                              (concat user "@" host)
                            host)
                          "|"))
            "sudo:root@" host
            ":" (or (file-remote-p file 'localname)
                    file))))
;; find-file-other-window does not works great on remote:
;; if given an absolute path on a remote host,
;; the path will be understood as a local file since no
;; tramp prefix is present, and bash does not care
;; about tramp prefixes.
;; Function assumes, that 'default-directory' variable will be set in
;; different way on local machine and on a remote machine.
;; Therefore, distinction  betwen local and remote path is made by invoking (file-remote-p default-directory).
;; Bellow we solve context before sending it to
;; ffow
(defun my/vterm--find-file-other-window-wrapper (file)
  "Help vterm find a FILE."
  (find-file-other-window (my/vterm--ffow-resolver file)))

(defun my/vterm--ffow-resolver (file)
  "Help vterm resolve FILE."
  (cond
   ;; "/sudo::"
   ;; doom--sudo-file-path do the trick for us
   ((s-starts-with-p "/sudo::" file)
    (doom--sudo-file-path
     (concat (file-remote-p default-directory)
             (substring-no-properties file 7))))
   ;; "/" means we want the "Relative root"
   ;; try appending the remote prefix if relevent
   ((s-starts-with-p "/" file)
    (concat (file-remote-p default-directory) file))
   ;; we got a relative path
   ;; we don't need to help ffow to find it
   (t
    file)))

;; The variable vterm-eval-cmds is a SERIOUSLY SENSIBLE variable !
;; Do not be the guy that adds RCE into their config !

;; Allow customed ffow to be called from vterm
;; ffow should be as safe as find-file which is already trusted
;; we append our resolver that only manipulate strings,
;; Proove me wrong but i think it's safe.
(add-to-list 'vterm-eval-cmds '("find-file-other-window"
                                my/vterm--find-file-other-window-wrapper))
