;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example init file for use with SmSn-mode

;; relace /path/to/smsn-mode with actual path to the SmSn-mode source directory
(let ((default-directory "/path/to/smsn-mode"))
      (normal-top-level-add-subdirs-to-load-path))

;; load the library
(require 'smsn-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; optional settings

;; default Gremlin Server connection settings
(defvar smsn-server-host "127.0.0.1")
(defvar smsn-server-port 8182)
(defvar smsn-server-protocol "websocket")  ;; alternative is "http"

;; default locationsfor graph input/output files
;;(defvar smsn-default-graphml-file "/path/to/mygraphdata/graph.xml")
;;(defvar smsn-default-graphml-file "/path/to/mygraphdata/graph.xml")
;;(defvar smsn-default-vertices-file "/path/to/mygraphdata/vertices.tsv")
;;(defvar smsn-default-edges-file "/path/to/mygraphdata/edges.tsv")
;;(defvar smsn-default-rdf-file "/path/to/mygraphdata/graph.nt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; additional useful shortcuts

(define-key smsn-mode-map (kbd "C-c C-t C-b a")   (smsn-search-in-browser
  "Amazon" "http://www.amazon.com/s?ie=UTF8&index=blended&link_code=qs&field-keywords="))
(define-key smsn-mode-map (kbd "C-c C-t C-b d")   (smsn-search-in-browser
  "Delicious" "http://www.delicious.com/search?p="))
(define-key smsn-mode-map (kbd "C-c C-t C-b e")   (smsn-search-in-browser
  "eBay" "http://www.ebay.com/sch/i.html?_nkw="))
(define-key smsn-mode-map (kbd "C-c C-t C-b g")   (smsn-search-in-browser
  "Google" "http://www.google.com/search?ie=UTF-8&q="))
(define-key smsn-mode-map (kbd "C-c C-t C-b m")   (smsn-search-in-browser
  "Google Maps" "http://maps.google.com/maps?q="))
(define-key smsn-mode-map (kbd "C-c C-t C-b s")   (smsn-search-in-browser
  "Google Scholar" "http://scholar.google.com/scholar?q="))
(define-key smsn-mode-map (kbd "C-c C-t C-b t")   (smsn-search-in-browser
  "Twitter" "http://twitter.com/#!/search/"))
(define-key smsn-mode-map (kbd "C-c C-t C-b w")   (smsn-search-in-browser
  "Wikipedia" "http://en.wikipedia.org/w/index.php?title=Special%3ASearch&search="))
(define-key smsn-mode-map (kbd "C-c C-t C-b y")   (smsn-search-in-browser
  "YouTube" "http://www.youtube.com/results?search_query="))

