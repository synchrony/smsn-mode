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

;; default locations for graph input/output files
;;(defvar smsn-default-graphml-file "/path/to/mygraphdata/graph.xml")
;;(defvar smsn-default-vertices-file "/path/to/mygraphdata/vertices.tsv")
;;(defvar smsn-default-edges-file "/path/to/mygraphdata/edges.tsv")
;;(defvar smsn-default-rdf-file "/path/to/mygraphdata/graph.nt")

;; enables "fuzzy" search
(defconst smsn-const-query-by-partial-title t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; additional useful shortcuts

;; Add or remove services as needed.
;; Usage example:
;;     C-c C-t C-b g -- search on the currently selected line of text on Google
;;     C-c C-r C-b g -- search on the title of the current buffer in Google
(defconst smsn-search-services (list
  (list "a" "Amazon" "http://www.amazon.com/s?ie=UTF8&index=blended&link_code=qs&field-keywords=")
  (list "d" "Delicious" "http://www.delicious.com/search?p=")
  (list "e" "eBay" "http://www.ebay.com/sch/i.html?_nkw=")
  (list "g" "Google" "http://www.google.com/search?ie=UTF-8&q=")
  (list "m" "Google Maps" "http://maps.google.com/maps?q=")
  (list "s" "Google Scholar" "http://scholar.google.com/scholar?q=")
  (list "t" "Twitter" "http://twitter.com/#!/search/")
  (list "w" "Wikipedia" "http://en.wikipedia.org/w/index.php?title=Special%3ASearch&search=")
  (list "y" "YouTube" "http://www.youtube.com/results?search_query=")))

