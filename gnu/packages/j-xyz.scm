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
  #:use-module (gnu packages j)
  )

;; https://github.com/jsoftware/arc_zlib.git
;; b88f4cab94af12157b33fb22ce30f59ea481a840
(define-public j-arc-zlib
  (let ((commit "f5e6a1138d4f481a4615cffd90cc01f704d0729b")
        (version "1.0.9"))
    (package
      (name "j-arc-zlib")
      (version version)
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/jitwit/arc_zlib.git")
           (commit commit)))
         (sha256
          (base32 "0cyqv22nkznlcxy8sjkvycnbiaa7mgfm45vlpjfrzh7vxrhrq3pa"))))
      (inputs `(("zlib" ,zlib)))
      (native-inputs `(("j" ,j-901)))
      (outputs '("out"))
      (build-system gnu-build-system)
      (arguments
       `(#:modules ((guix build gnu-build-system)
                    (guix build utils)
                    (ice-9 popen))
         #:phases
         (modify-phases %standard-phases
           (replace 'configure
             (lambda _
               (substitute* '("zlib.ijs")
                 (("zlib=: .+$")
                  (string-append "zlib=: '"
                                 (search-path
                                  (search-path-as-string->list
                                   (getenv "LIBRARY_PATH"))
                                  "libz.so")
                                 "'\n")))
               #t))
           (delete 'check)
           (delete 'build)
           (replace 'install
             (lambda _
               (let ((out (string-append (assoc-ref %outputs "out")
                                         "/share/j/addons/arc/zlib")))
                 (copy-recursively "." out)
                 #t))))))
      (home-page "https://github.com/jsoftware/arc_zlib")
      (synopsis "Interface with zlib")
      (description "This J addon provides an interface to zlib.")
      (license expat))))

;; 171vw8lzw0f0njrr2d0nd0vn7crv0q5yrysz8wa7ky6kq92g2w75
;; https://github.com/cdburke/convert_pjson.git
(define-public j-convert-pjson
  (let ((commit "a64defe9adb24a0350517ab99121e8c75259983e")
        (version "1.0.23"))
    (package
      (name "j-convert-pjson")
      (version version)
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/cdburke/convert_pjson.git")
           (commit commit)))
         (sha256
          (base32 "1km259hnvc1qwxhvb6nz7pk79jz5rn62g43yhn6ma5bvfz5hj35r"))))
      (native-inputs `(("j" ,j-901)))
      (outputs '("out"))
      (build-system gnu-build-system)
      (arguments
       `(#:modules ((guix build gnu-build-system)
                    (guix build utils)
                    (ice-9 popen))
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'check)
           (delete 'build)
           (replace 'install
             (lambda _
               (let ((out (string-append (assoc-ref %outputs "out")
                                         "/share/j/addons/convert/pjson")))
                 (copy-recursively "." out)
                 #t))))))
      (home-page "https://github.com/cdburke/convert_pjson")
      (synopsis "json library for J")
      (description "This J addon provides json serialization from within J.")
      (license expat))))

;; edcffe8d7847e9941d14f6838110a068e0a89102
;; https://github.com/jsoftware/graphics_bmp.git
(define-public j-graphics-bmp
  (let ((commit "edcffe8d7847e9941d14f6838110a068e0a89102")
        (version "1.0.14"))
    (package
      (name "j-graphics-bmp")
      (version version)
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/jsoftware/graphics_bmp.git")
           (commit commit)))
         (sha256
          (base32 "00k417ia5fvszmb4pnd4japrn8i15ync8n4fk1l7i7fbafinxrnr"))))
      (native-inputs `(("j" ,j-901)))
      (outputs '("out"))
      (build-system gnu-build-system)
      (arguments
       `(#:modules ((guix build gnu-build-system)
                    (guix build utils)
                    (ice-9 popen))
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'check)
           (delete 'build)
           (replace 'install
             (lambda _
               (let ((out (string-append (assoc-ref %outputs "out")
                                         "/share/j/addons/graphics/bmp")))
                 (copy-recursively "." out)
                 #t))))))
      (home-page "https://github.com/jsoftware/graphics_bmp")
      (synopsis "Utilities for *.bmp files")
      (description "Utilities for *.bmp files")
      (license expat))))

;; https://github.com/jsoftware/graphics_color.git
;; 2bb8578c370fd2f25b118ae8cba11153c4687eab
(define-public j-graphics-color
  (let ((commit "2bb8578c370fd2f25b118ae8cba11153c4687eab")
        (version "1.0.19"))
    (package
      (name "j-graphics-color")
      (version version)
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/jsoftware/graphics_color.git")
           (commit commit)))
         (sha256
          (base32 "155m56f0d268jc8g9yc6fw5l8mnx2sm408if865h0a7682mpgqrb"))))
      (native-inputs `(("j" ,j-901)))
      (outputs '("out"))
      (build-system gnu-build-system)
      (arguments
       `(#:modules ((guix build gnu-build-system)
                    (guix build utils)
                    (ice-9 popen))
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'check)
           (delete 'build)
           (replace 'install
             (lambda _
               (let ((out (string-append (assoc-ref %outputs "out")
                                         "/share/j/addons/graphics/color")))
                 (copy-recursively "." out)
                 #t))))))
      (home-page "https://github.com/jsoftware/graphics_color")
      (synopsis "Color tables and related scripts")
      (description "Color tables and related scripts")
      (license expat))))

;; ;; https://github.com/jsoftware/graphics_png.git
;; ;; 2767c9b8efea71c38b0d8433bd58aba360ea464a
;; (define-public j-graphics-png
;;   (let ((commit "2767c9b8efea71c38b0d8433bd58aba360ea464a")
;;         (version "1.0.28"))
;;     (package
;;       (name "j-graphics-png")
;;       (version version)
;;       (source
;;        (origin
;;          (method git-fetch)
;;          (uri
;;           (git-reference
;;            (url "https://github.com/jsoftware/graphics_png.git")
;;            (commit commit)))
;;          (sha256
;;           (base32 "1i5i9x7am36dr58bvlhydyp3bhmhbgg355k9jfddjylbrsnb7rc9"))))
;;       (inputs `(("j" ,j-901)
;;                 ("j-arc-zlib" ,j-arc-zlib)))
;;       ;; (propagated-inputs `(("j-arc-zlib" ,j-arc-zlib)))
;;       (outputs '("out"))
;;       (build-system gnu-build-system)
;;       (arguments
;;        `(#:modules ((guix build gnu-build-system) (guix build utils))
;;          #:tests? #f
;;          #:phases
;;          (modify-phases %standard-phases
;;            (delete 'configure)
;;            (delete 'check)
;;            (delete 'build)
;;            (replace 'install
;;              (lambda _
;;                (let* ((out (string-append (assoc-ref %outputs "out")
;;                                           "/share/j/addons/graphics/png")))
;;                  (install-file "png.ijs" out)
;;                  #t))))))
;;       (home-page "https://github.com/jsoftware/arc_zlib")
;;       (synopsis "Interface with zlib")
;;       (description "This J addon provides an interface to zlib.")
;;       (license expat))))
;; 
;; ;; bbfc957fc4ddf90231c4a06239cbc85c67cc2769
;; ;; https://github.com/jsoftware/general_misc.git
;; (define j-general-misc
;;   (let ((commit "bbfc957fc4ddf90231c4a06239cbc85c67cc2769")
;;         (version "2.5.3"))
;;     (package
;;       (name "j-general-misc")
;;       (version version)
;;       (source
;;        (origin
;;          (method git-fetch)
;;          (uri
;;           (git-reference
;;            (url "https://github.com/jsoftware/general_misc.git")
;;            (commit commit)))
;;          (sha256
;;           (base32 "0pp71i7ylyn4nv43vagh0h2y1yb2dx0py97bscxykk04xavcvx6j"))))
;;       (inputs `(("j" ,j-901)))
;;       (build-system gnu-build-system)
;;       (arguments
;;        `(#:modules ((guix build gnu-build-system) (guix build utils))
;;          #:tests? #f
;;          #:phases
;;          (modify-phases %standard-phases
;;            (delete 'configure)
;;            (delete 'check)
;;            (delete 'build)
;;            (replace 'install
;;              (lambda _
;;                (let* ((out (string-append (assoc-ref %outputs "out")
;;                                           "/share/j/addons/graphics/png")))
;;                  (install-file "png.ijs" out)
;;                  #t))))))
;;       (home-page "https://github.com/jsoftware/arc_zlib")
;;       (synopsis "Interface with zlib")
;;       (description "This J addon provides an interface to zlib.")
;;       (license expat))))
;; 
