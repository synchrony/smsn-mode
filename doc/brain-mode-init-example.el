;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Brain-mode

;; copy or link to Brain-mode's ./lisp directory here
(let ((default-directory "~/.emacs.d/elisp/"))
      (normal-top-level-add-subdirs-to-load-path))

;; load the library
(require 'brain-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; optional settings

;; overrride default Gremlin Server connection settings
(defvar brain-server-host "127.0.0.1")
(defvar brain-server-port 8182)
(defvar brain-server-protocol "websocket")

;; export and import graph data in the VCS format from here
(defvar brain-default-vcs-file "/opt/brains/git-smsn")

;; export the graph to here, or populate an empty graph from here
(defvar brain-default-graphml-file "/tmp/arthurdent.xml")

;; default location for dumps of tab-separated vertex and edge files
;; vertex files contain the properties of each atom
;; edge files are parent/child adjacency lists
(defvar brain-default-vertices-file "/tmp/arthurdent-vertices.tsv")
(defvar brain-default-edges-file "/tmp/arthurdent-edges.tsv")

;; RDF output is written here
(defvar brain-default-rdf-file "/tmp/arthurdent.nt")
