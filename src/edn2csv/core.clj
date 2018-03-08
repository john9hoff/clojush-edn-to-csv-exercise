(ns edn2csv.core
  (require [clojure.core.reducers :as r]
           [clojure.edn :as edn]
           [clojure.java.io :as io]
           [clojure.pprint :as pp]
           [iota]
           [me.raynes.fs :as fs])
  (:gen-class))

; The header line for the Individuals CSV file
(def individuals-header-line "UUID:ID(Individual),Generation:int,Location:int,:LABEL")
(def parentOfEdges-header-line ":START_ID(Individual), GeneticOperator:string, :END_ID(Individual), :TYPE")
(def semantics-header-line "UUID:ID(Semantics), TotalError:int, :LABEL")

; Generate UUIDs for semantics
(defn generate-uuid [] (str (java.util.UUID/randomUUID)))

; Atom for semantics
(def semantics-atom (atom {}))
; Ignores (i.e., returns nil) any EDN entries that don't have the
; 'clojure/individual tag.
(defn individual-reader
    [t v]
    (when (= t 'clojush/individual) v))

; I got this from http://yellerapp.com/posts/2014-12-11-14-race-condition-in-clojure-println.html
; It prints in a way that avoids weird interleaving of lines and items.
; In several ways it would be better to use a CSV library like
; clojure.data.csv, but that won't (as written) avoid the interleaving
; problems, so I'm sticking with this approach for now.
(defn safe-println [output-stream & more]
  (.write output-stream (str (clojure.string/join "," more) "\n")))

; This prints out the relevant fields to the CSV filter
; and then returns 1 so we can count up how many individuals we processed.
; (The counting isn't strictly necessary, but it gives us something to
; fold together after we map this across the individuals; otherwise we'd
; just end up with a big list of nil's.)
(defn print-individual-to-csv
  [csv-file line]
  (as-> line $
    (map $ [:uuid :generation :location])
    (concat $ ["Individual"])
    (apply safe-println csv-file $))
  1)

(defn print-semantics-to-csv
  [csv-file line]
  (when-not (contains? @semantics-atom (get line :errors))
    (let [semantic-uuid (generate-uuid)
          values [semantic-uuid (get line :total-error) "Semantics"]]
        (swap! semantics-atom assoc (get line :errors) (get line :total-error))
        (apply safe-println csv-file values)))
  1)


(defn print-parentOfEdges-to-csv
  [csv-file line]
  (as-> line $
    (map $ [:parent-uuids :genetic-operators :uuid])
    (concat $ ["PARENT_OF"])
    (apply safe-println csv-file $))
  1)

(defn edn->csv-sequential-parentOfEdges [edn-file csv-file]
  (with-open [out-file (io/writer csv-file)]
    (safe-println out-file parentOfEdges-header-line)
    (->>
      (line-seq (io/reader edn-file))
      ; Skip the first line because it's not an individual
      (drop 1)
      (map (partial edn/read-string {:default individual-reader}))
      (map (partial print-parentOfEdges-to-csv out-file))
      (reduce +)
      )))

(defn edn->csv-sequential-semantics [edn-file csv-file]
  (with-open [out-file (io/writer csv-file)]
    (safe-println out-file semantics-header-line)
    (->>
      (line-seq (io/reader edn-file))
      ; Skip the first line because it's not an individual
      (drop 1)
      (map (partial edn/read-string {:default individual-reader}))
      (map (partial print-semantics-to-csv out-file))
      (reduce +)
      )))

(defn edn->csv-sequential [edn-file csv-file]
  (with-open [out-file (io/writer csv-file)]
    (safe-println out-file individuals-header-line)
    (->>
      (line-seq (io/reader edn-file))
      ; Skip the first line because it's not an individual
      (drop 1)
      (map (partial edn/read-string {:default individual-reader}))
      (map (partial print-individual-to-csv out-file))
      (reduce +)
      )))

(defn edn->csv-pmap-parentOfEdges [edn-file csv-file]
  (with-open [out-file (io/writer csv-file)]
    (safe-println out-file parentOfEdges-header-line)
    (->>
      (line-seq (io/reader edn-file))
      ; Skip the first line because it's not an individual
      (drop 1)
      (pmap (fn [line]
        (print-parentOfEdges-to-csv out-file (edn/read-string {:default individual-reader} line))
        1))
      count
      )))

(defn edn->csv-pmap [edn-file csv-file]
  (with-open [out-file (io/writer csv-file)]
    (safe-println out-file individuals-header-line)
    (->>
      (line-seq (io/reader edn-file))
      ; Skip the first line because it's not an individual
      (drop 1)
      (pmap (fn [line]
        (print-individual-to-csv out-file (edn/read-string {:default individual-reader} line))
        1))
      count
      )))

(defn edn->csv-pmap-semantics [edn-file csv-file]
  (with-open [out-file (io/writer csv-file)]
    (safe-println out-file semantics-header-line)
    (->>
      (line-seq (io/reader edn-file))
      ; Skip the first line because it's not an individual
      (drop 1)
      (pmap (fn [line]
        (print-semantics-to-csv out-file (edn/read-string {:default individual-reader} line))
        1))
      count
      )))

(defn edn->csv-reducers [edn-file csv-file]
  (with-open [out-file (io/writer csv-file)]
    (safe-println out-file individuals-header-line)
    (->>
      (iota/seq edn-file)
      (r/map (partial edn/read-string {:default individual-reader}))
      ; This eliminates empty (nil) lines, which result whenever
      ; a line isn't a 'clojush/individual. That only happens on
      ; the first line, which is a 'clojush/run, but we still need
      ; to catch it. We could do that with `r/drop`, but that
      ; totally kills the parallelism. :-(
      (r/filter identity)
      (r/map (partial print-individual-to-csv out-file))
      (r/fold +)
      )))

(defn build-individual-csv-filename
  [edn-filename strategy]
  (str (fs/parent edn-filename)
       "/"
       (fs/base-name edn-filename ".edn")
       (if strategy
         (str "_" strategy)
         "_sequential")
       "_Individuals.csv"))

(defn build-parentOfEdges-csv-filename
 [edn-filename strategy]
 (str (fs/parent edn-filename)
      "/"
      (fs/base-name edn-filename ".edn")
      (if strategy
        (str "_" strategy)
        "_sequential")
      "_ParentOfEdges.csv"))

(defn build-semantics-csv-filename
  [edn-filename strategy]
  (str (fs/parent edn-filename)
       "/"
       (fs/base-name edn-filename ".edn")
       (if strategy
         (str "_" strategy)
         "_sequential")
       "_Semantics.csv"))

(defn -main
  [edn-filename & [strategy]]
  (let [individual-csv-file (build-individual-csv-filename edn-filename strategy)]
    (time
      (condp = strategy
        "sequential" (edn->csv-sequential edn-filename individual-csv-file)
        "pmap" (edn->csv-pmap edn-filename individual-csv-file)
        "reducers" (edn->csv-reducers edn-filename individual-csv-file)
        (edn->csv-sequential edn-filename individual-csv-file))))


  (let [parentOfEdges-csv-file (build-parentOfEdges-csv-filename edn-filename strategy)]
    (time
      (condp = strategy
        "sequential" (edn->csv-sequential-parentOfEdges edn-filename parentOfEdges-csv-file)
        "pmap" (edn->csv-pmap-parentOfEdges edn-filename parentOfEdges-csv-file)
        (edn->csv-sequential-parentOfEdges edn-filename parentOfEdges-csv-file))))

  (let [semantics-csv-file (build-semantics-csv-filename edn-filename strategy)]
    (time
      (condp = strategy
        "pmap" (edn->csv-pmap-semantics edn-filename semantics-csv-file)
        (edn->csv-pmap-semantics edn-filename semantics-csv-file))))
  ; Necessary to get threads spun up by `pmap` to shutdown so you get
  ; your prompt back right away when using `lein run`.
  (shutdown-agents))
