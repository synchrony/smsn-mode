;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example init file for use with SmSn-mode

;; copy or link to SmSn-mode's ./lisp directory here
(let ((default-directory "~/.emacs.d/elisp/"))
      (normal-top-level-add-subdirs-to-load-path))

;; load the library
(require 'smsn-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; optional settings

;; overrride default Gremlin Server connection settings
(defvar smsn-server-host "127.0.0.1")
(defvar smsn-server-port 8182)
(defvar smsn-server-protocol "websocket")

;; export and import graph data in the VCS format from here
(defvar smsn-default-vcs-file "/mygraphdata/git-smsn")

;; export the graph to here, or populate an empty graph from here
(defvar smsn-default-graphml-file "/mygraphdata/graph.xml")

;; default location for dumps of tab-separated vertex and edge files
;; vertex files contain the properties of each atom
;; edge files are parent/child adjacency lists
(defvar smsn-default-vertices-file "/mygraphdata/vertices.tsv")
(defvar smsn-default-edges-file "/mygraphdata/edges.tsv")

;; RDF output is written here
(defvar smsn-default-rdf-file "/mygraphdata/graph.nt")
