(ns evolutionary-metrics.app.exclude-commits)

(defn- filter-commits-matching
  [exclude-commits commits]
  (let [cs (set exclude-commits)]
    (remove (comp cs :rev) commits)))

(defn by-commit-hash
  "Filters the log by removing specific commits.

   This filter is typically applied then a codebase was imported
   into Git and the initial commit was done by and individual.
   That initial commit will obscure the ownership metrics, so here's
   a way to filter the evolutionary data."
  [{:keys [exclude-commits]} commits]
  (if (seq exclude-commits)
    (filter-commits-matching exclude-commits commits)
    commits))
