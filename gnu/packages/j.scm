;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Joseph Novakovich <josephnovakovich@gmail.com>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gnu packages j)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages readline))

(define-public j-901
  (let ((commit "370058d8db066ccf54fa088bb2a4d3ac3283471b")
        (jplatform "linux")
        (jversion "901")
        (jtype "beta"))
    (package
      (name "j")
      (version jversion)
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/jsoftware/jsource.git")
           (commit commit)))
         (sha256
          (base32 "0ihqcxa90aa375r93x0zlnwnxgk06gf5g6z7lxpm8a7kbcwp0iai"))))
      (build-system gnu-build-system)
      (inputs `(("bash" ,bash)
                ("readline" ,readline)
                ("bc" ,bc)
                ("libedit" ,libedit)
                ("gcc" ,gcc-9)))
      (outputs '("out"))
      (arguments
       `(#:modules ((guix build gnu-build-system) (guix build utils))
         #:tests? #f ;; todo!
         #:phases
         (modify-phases %standard-phases
           (replace 'configure
             (lambda _
               (setenv "HOME" (getenv "TEMP"))
               (let* ((jgit (getcwd))
                      (jbld (string-append (getenv "HOME") "/jbld"))
                      (jplatform "linux")
                      (jsuffix "so")
                      (CC "gcc")
                      (tsu (string-append jgit "/test/tsu.ijs"))
                      (j32 (string-append jbld "/j32/bin/jconsole " tsu))
                      (j64 (string-append jbld "/j64/bin/jconsole " tsu))
                      (j64avx (string-append jbld "/j64/bin/jconsole -lib libjavx."
                                             jsuffix " " tsu))
                      (j64avx2 (string-append jbld "/j64/bin/jconsole -lib libjavx2."
                                              jsuffix " " tsu))
                      (jmake (string-append jgit "/make"))
                      (out (assoc-ref %outputs "out"))
                      (guix-profile-j-share "'/share/j',~2!:5'J_INSTALL'")
                      (j-pre-install (string-append jbld "/j64")))
                 (for-each setenv
                           (list "jgit" "jbld" "jplatform" "jsuffix" "CC"
                                 "tsu" "j32" "j64" "j64avx" "j64avx2" "jmake"
                                 "J_INSTALL")
                           (list jgit jbld jplatform jsuffix CC
                                 tsu j32 j64 j64avx j64avx2 jmake
                                 j-pre-install))
                 (with-output-to-file "jsrc/jversion.h"
                   (lambda ()
                     (display "#define jversion  ") (write ,jversion)  (newline)
                     (display "#define jplatform ") (write ,jplatform) (newline)
                     (display "#define jtype     ") (write ,jtype)     (newline)
                     (display "#define jlicense  ") (write "GPL3")     (newline)
                     (display "#define jbuilder  ") (write "guix")     (newline)))
                 (substitute* (list (string-append jgit "/jlibrary/bin/profile.ijs"))
                   (("'/usr/share/j/9.01'") guix-profile-j-share))
                 (string-append jbld "/j64/bin")
                 (mkdir-p (string-append jbld "/j64/bin"))
                 (copy-recursively (string-append jgit "/jlibrary")
                                   (string-append jbld "/j64/share/j"))
                 (install-file (string-append jgit "/jlibrary/bin/profile.ijs")
                               (string-append jbld "/j64/bin"))
                 #t)))
           (replace 'build
             (lambda _
               (invoke "make/build_all.sh" "j64")
               #t))
           (replace 'check
             (lambda _
               (system "echo \"RECHO ddall\" | $j64avx2")))
           (replace 'install
             (lambda _
               (let ((out (assoc-ref %outputs "out"))
                     (jbld (getenv "jbld")))
                 (copy-recursively jbld out)
                 #t))))))
      (synopsis "APL Dialect")
      (description "Terse, array-based language originally developed by KenIverson and Roger Hui.")
      (home-page "https://www.jsoftware.com/")
      (license gpl3))))
