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

(define-module (gnu packages j-xyz)
  #:use-module (ice-9 popen)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) :select (expat))
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages j)
  )

(define (read-manifest val)
  "Read the given J name's value with jconsole to a string in scheme."
  (define command
    (format #f "echo \"(2!:55)(0 0 $ 1!:2&2) ~a\" | jconsole manifest.ijs" val))
  (define out (open-pipe command OPEN_READ))
  (let loop ((x (read-char out)) (xs '()))
    (if (eof-object? x)
	(list->string (reverse xs))
	(loop (read-char out) (cons x xs)))))

(define (manifest-files)
  (string-tokenize (read-manifest 'FILES)))

(define (manifest-version)
  (car (string-tokenize (read-manifest 'VERSION))))

(define (manifest-depends)
  (string-tokenize (read-manifest 'DEPENDS)))

;; https://github.com/jsoftware/arc_zlib.git
;; b88f4cab94af12157b33fb22ce30f59ea481a840
(define-public j-arc-zlib
  (let ((commit "b88f4cab94af12157b33fb22ce30f59ea481a840")
        (version "1.0.8"))
    (package
      (name "j-arc-zlib")
      (version version)
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/jsoftware/arc_zlib.git")
           (commit commit)))
         (sha256
          (base32 "0pp71i7ylyn4nv43vagh0h2y1yb2dx0py97bscxykk04xavcvx6j"))))
      (inputs `(("j" ,j-901)
                ("zlib" ,zlib)))
      (propagated-inputs `(("zlib" ,zlib)))
      (outputs '("out"))
      (build-system gnu-build-system)
      (arguments
       `(#:modules ((guix build gnu-build-system) (guix build utils))
         #:tests? #f
         #:phases
         (modify-phases %standard-phases
           (replace 'configure
             (lambda _
               (substitute* '("zlib.ijs")
                 (("zlib=: ") "zlib=: '/lib/libz.so',~2!:5'GUIX_PROFILE' NB. ")) ;; todo find less hacky way
               #t))
           (delete 'check)
           (delete 'build)
           (replace 'install
             (lambda _
               (let* ((out (string-append (assoc-ref %outputs "out")
                                          "/share/j/addons/arc/zlib")))
                 ;; (system "echo \"exit echo FOLDER\" | jconsole manifest.ijs") todo explore porting j addon build system to a guix one
                 (for-each (lambda (f)
                             (install-file f out))
                           '("zlib.ijs"
                             "manifest.ijs"
                             "readme.txt"
                             "history.txt"))
                 #t))))))
      (home-page "https://github.com/jsoftware/arc_zlib")
      (synopsis "Interface with zlib")
      (description "This J addon provides an interface to zlib.")
      (license expat))))

;; https://github.com/jsoftware/graphics_png.git
;; 2767c9b8efea71c38b0d8433bd58aba360ea464a
(define-public j-graphics-png
  (let ((commit "2767c9b8efea71c38b0d8433bd58aba360ea464a")
        (version "1.0.28"))
    (package
      (name "j-graphics-png")
      (version version)
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/jsoftware/graphics_png.git")
           (commit commit)))
         (sha256
          (base32 "1i5i9x7am36dr58bvlhydyp3bhmhbgg355k9jfddjylbrsnb7rc9"))))
      (inputs `(("j" ,j-901)
                ("j-arc-zlib" ,j-arc-zlib)))
      (propagated-inputs `(("j-arc-zlib" ,j-arc-zlib)))
      (outputs '("out"))
      (build-system gnu-build-system)
      (arguments
       `(#:modules ((guix build gnu-build-system) (guix build utils))
         #:tests? #f
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'check)
           (delete 'build)
           (replace 'install
             (lambda _
               (let* ((out (string-append (assoc-ref %outputs "out")
                                          "/share/j/addons/graphics/png")))
                 (install-file "png.ijs" out)
                 #t))))))
      (home-page "https://github.com/jsoftware/arc_zlib")
      (synopsis "Interface with zlib")
      (description "This J addon provides an interface to zlib.")
      (license expat))))

;; bbfc957fc4ddf90231c4a06239cbc85c67cc2769
;; https://github.com/jsoftware/general_misc.git
(define j-general-misc
  (let ((commit "bbfc957fc4ddf90231c4a06239cbc85c67cc2769")
        (version "2.5.3"))
    (package
      (name "j-general-misc")
      (version version)
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/jsoftware/general_misc.git")
           (commit commit)))
         (sha256
          (base32 "0pp71i7ylyn4nv43vagh0h2y1yb2dx0py97bscxykk04xavcvx6j"))))
      (inputs `(("j" ,j-901)))
      (build-system gnu-build-system)
      (arguments
       `(#:modules ((guix build gnu-build-system) (guix build utils))
         #:tests? #f
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'check)
           (delete 'build)
           (replace 'install
             (lambda _
               (let* ((out (string-append (assoc-ref %outputs "out")
                                          "/share/j/addons/graphics/png")))
                 (install-file "png.ijs" out)
                 #t))))))
      (home-page "https://github.com/jsoftware/arc_zlib")
      (synopsis "Interface with zlib")
      (description "This J addon provides an interface to zlib.")
      (license expat))))
