SmSn-mode CHANGELOG
========================================

SmSn-mode 1.5 SNAPSHOT (not yet released)
----------------------------------------
****************************************
* Added key bindings for search on buffer title
****************************************

SmSn-mode 1.4 (Pogo Stilts)
----------------------------------------
****************************************
* Added optional support for partial search
* Use generalized data sources configured on the server side
* Replaced @sharability with @source for data management and visibility
* Refine cursor placement
* Tighten the on-screen footprint of the property-setting shortcuts
* Harmonize smsn-push-view shortcut with the preexisting Emacs shortcut for save-buffer
* Added bury-line and float-line shortcuts
* Cap the inherited tree depth at three
* Command to open a view of an atom in another window (in the same frame)
* Added a command for the Git history view
* Added a command to open a view after prompting for an id
* Default VCS import/export file no longer needs to be defined
* Renamed brain-mode to smsn-mode
* Eliminated max sharability and max weight from filters
****************************************

Brain-mode 1.3 (Join the Army)
----------------------------------------
****************************************
* Added support for WebSocket in addition to HTTP for client-server interaction
* Color highlighting in meta columns
* Adjusted to Semantic Synchrony data model changes
* Added editable "page" views
* Added a command to search on the atom at point
* Display parent and child counts as metadata columns
* Added a 'ping' command
****************************************

Brain-mode 1.2 (Into the Future)
----------------------------------------
****************************************
* Added client support for VCS (version control system) reader and writer
* Refactored and improved buffer context management
* Added support for the 'create new atom' action
* Adjusted to refactored SmSn Server actions
* Simplified Emacs configuration
* Added move mode shortcuts to set properties
* Migrated to SmSn 1.2, which uses Apache TinkerPop's Gremlin Server
* Color command prompts by minimum sharability
* Prompt before push in move mode
****************************************

SmSn 1.1 (More Brains)
----------------------------------------
****************************************
* Created a unified BrainReader/BrainWriter framework for graph I/O
* Minimized TinkerPop2 dependencies
* Moved Brain-mode to a separate, GPL-licensed project
* Accepted first PRs to Brain-mode
* Added a Freeplane reader
* Made Brain-mode into an Emacs major mode
* Adapted to Java 8, Sesame 4, and RDF 1.1
* Updated the Monitron ontology and controller
* Added a LaTeX writer
* Added a GraphML reader
* Added "Brainstream" (eyes-free input) support
* Added support for shortcut-based search
* Added support for acronym-based search
* Added support for cooperative gesture recognition
* Added the UbiKeyboard application
* Added an RDF writer
* Created v2.0 of Extend-o-Brain inference support
* Added Linked Data and OSC support to the coordinator
* Added a Ripple environment for use with the Typeatron
****************************************

SmSn 1.0 (Semantic Gadgets)
----------------------------------------
****************************************
* Added a GraphML writer
* Integrated the Typeatron with Brain-mode via emacsclient
* Created a framework for inter-device discovery and communication
* Added type inference annotations to Brain-mode
* Added Extend-o-Brain support to the Brainstem
* Added an event processor to Extend-o-Brain
* Added a type inference and RDF export framework for Extend-o-Brain
* Added Bluetooth and OSC support to the devices
* Added support for single-device gesture recognition
* Created an Extend-o-Hand controller in Java
* Created a Typeatron controller in Java
* Created Max/MSP control panels for the devices
* Created Arduino-based libraries for the devices
* Created Extend-o-Hand
* Created the Monomanual Typeatron
****************************************

SmSn 0.x (Braaains)
----------------------------------------
****************************************
* Prototype design of the Monomanual Typeatron
* Added Emacspeak support to TinkerNotes (Brain-mode)
* Moved to an ordered, list-based data model in Extend-o-Brain
* Added RDF output to the Monitron
* Created the Omnisensory Monitron (a multi-sensor sampling device)
* Added a service to find graph roots
* Added a PageRank service for Extend-o-Brain
* Added Extend-o-Brain support for @alias URIs
* Added an activity log
* Added a Chrome bookmarking extension for use with Extend-o-Brain
* Made various deep changes to the data model, settling on a digraph model
* Added tab-separated writers for vertices and edges
* Created initial Extend-o-Brain services for view, update, history, and query
* Added a RESTful server for use with Emacs
* Created the TinkerNotes (Brain-mode) Emacs library
* Added storage, query and update support
* Implemented the initial "association graphs" data model
* Created pre- Extend-o-Brain applications, later moved to other repositories
* Created an Android library (later called the Brainstem)
* Created MyOtherBrain (Extend-o-Brain) prototypes with Java, Flash/Flex, and JavaScript
* Created a personal knowledge base ontology
****************************************

