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
      (build-system gnu-build-system)
      (arguments
       `(#:modules ((guix build gnu-build-system) (guix build utils))
         #:tests? #f
         #:phases
         (modify-phases %standard-phases
           (replace 'configure (lambda _ #t))
           (replace 'build     (lambda _ #t))
           (replace 'check     (lambda _ #t))
           (replace 'install
             (lambda _
               (let* ((out (assoc-ref %outputs "out")))
                 (system "echo \"exit echo FOLDER\" | jconsole manifest.ijs")
                 (copy-recursively "." (string-append out "/share/j/addons/arc/zlib"))
                 #t))))))
      (inputs `(("j" ,j-901)
                ("zlib" ,zlib)))
      (home-page "https://github.com/jsoftware/arc_zlib")
      (synopsis "Interface with zlib")
      (description "This J addon provides an interface to zlib.")
      (license expat))))
