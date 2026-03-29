(deftemplate assessment
   ; Store all input data for one person
   (slot gender)
   (slot q1 (type INTEGER))
   (slot q2 (type INTEGER))
   (slot q3 (type INTEGER))
   (slot q4 (type INTEGER))
   (slot q5 (type INTEGER))
   (slot q6 (type INTEGER))
   (slot q7 (type INTEGER))
)

(deftemplate final-score
   ; Store total score
   (slot value (type INTEGER))
)

(deftemplate final-result
   ; Store final classification and risk level
   (slot status)
   (slot risk)
)

; Get gender from user
(deffunction get-gender ()
   (printout t crlf "Enter gender (F/M): ")
   (bind ?gender-input (read))

   (while (and (neq ?gender-input F)
               (neq ?gender-input M)
               (neq ?gender-input f)
               (neq ?gender-input m)) do
      (printout t "Invalid input. Please enter F or M: ")
      (bind ?gender-input (read))
   )

   (if (eq ?gender-input f) then (bind ?gender-input F))
   (if (eq ?gender-input m) then (bind ?gender-input M))

   (return ?gender-input)
)

; Get score for each question
(deffunction get-score (?question-text)
   (printout t ?question-text)
   (bind ?user-score (read))

   (while (or (not (integerp ?user-score)) (< ?user-score 0) (> ?user-score 3)) do
      (printout t "Invalid input. Please enter 0 to 3: ")
      (bind ?user-score (read))
   )

   (return ?user-score)
)

(deffacts startup
   (start)
)

; Collect all user input
(defrule collect-input-data
   ?start-fact <- (start)
   =>
   (retract ?start-fact)

   (bind ?gender (get-gender))

   (printout t crlf "Please answer the following questions (0-3):" crlf)
   (printout t "0 = Not at all, 1 = Slightly, 2 = Moderately, 3 = Severely" crlf crlf)

   (bind ?score1 (get-score "Q1. I know subject's HIU: "))
   (bind ?score2 (get-score "Q2. Subject prefers HIU to socializing: "))
   (bind ?score3 (get-score "Q3. I am concerned about subject's HIU: "))
   (bind ?score4 (get-score "Q4. HIU impairs subject's health, hygiene, and eating pattern: "))
   (bind ?score5 (get-score "Q5. Subject avoids other activities: "))
   (bind ?score6 (get-score "Q6. Subject tried to decrease HIU but failed: "))
   (bind ?score7 (get-score "Q7. HIU negatively impacts subject's school/job performance: "))

   (assert
      (assessment
         (gender ?gender)
         (q1 ?score1)
         (q2 ?score2)
         (q3 ?score3)
         (q4 ?score4)
         (q5 ?score5)
         (q6 ?score6)
         (q7 ?score7)))
)

; Calculate total score
(defrule calculate-final-score
   (assessment
      (q1 ?score1)
      (q2 ?score2)
      (q3 ?score3)
      (q4 ?score4)
      (q5 ?score5)
      (q6 ?score6)
      (q7 ?score7))
   =>
   (bind ?sum-score (+ ?score1 ?score2 ?score3 ?score4 ?score5 ?score6 ?score7))
   (assert (final-score (value ?sum-score)))
   (printout t crlf "Total Score = " ?sum-score crlf crlf)
)

; =========================
; FINAL CLASSIFICATION
; =========================

; Female normal
(defrule female-normal
   (assessment (gender F))
   (final-score (value ?sum-score&:(< ?sum-score 8)))
   =>
   (assert (final-result (status NO-HIU) (risk NORMAL)))
)

; Female low HIU risk
(defrule female-low
   (assessment (gender F))
   (final-score (value ?sum-score&:(and (>= ?sum-score 8) (<= ?sum-score 11))))
   =>
   (assert (final-result (status HIU) (risk LOW)))
)

; Female moderate HIU risk
(defrule female-moderate
   (assessment (gender F))
   (final-score (value ?sum-score&:(and (>= ?sum-score 12) (<= ?sum-score 16))))
   =>
   (assert (final-result (status HIU) (risk MODERATE)))
)

; Female high HIU risk
(defrule female-high
   (assessment (gender F))
   (final-score (value ?sum-score&:(and (>= ?sum-score 17) (<= ?sum-score 21))))
   =>
   (assert (final-result (status HIU) (risk HIGH)))
)

; Male normal
(defrule male-normal
   (assessment (gender M))
   (final-score (value ?sum-score&:(< ?sum-score 7)))
   =>
   (assert (final-result (status NO-HIU) (risk NORMAL)))
)

; Male low HIU risk
(defrule male-low
   (assessment (gender M))
   (final-score (value ?sum-score&:(and (>= ?sum-score 7) (<= ?sum-score 11))))
   =>
   (assert (final-result (status HIU) (risk LOW)))
)

; Male moderate HIU risk
(defrule male-moderate
   (assessment (gender M))
   (final-score (value ?sum-score&:(and (>= ?sum-score 12) (<= ?sum-score 16))))
   =>
   (assert (final-result (status HIU) (risk MODERATE)))
)

; Male high HIU risk
(defrule male-high
   (assessment (gender M))
   (final-score (value ?sum-score&:(and (>= ?sum-score 17) (<= ?sum-score 21))))
   =>
   (assert (final-result (status HIU) (risk HIGH)))
)

; =========================
; PRINT FINAL RESULT
; =========================

(defrule print-normal-result
   (final-result (status NO-HIU) (risk NORMAL))
   (assessment (gender ?g))
   (final-score (value ?sum-score))
   =>
   (printout t "Result: No HIU detected." crlf)
   (printout t "Risk Level: Normal" crlf)
   (if (eq ?g F)
      then
         (printout t "Note: Score is below the female HIU threshold." crlf crlf)
      else
         (printout t "Note: Score is below the male HIU threshold." crlf crlf))
)

(defrule print-hiu-result
   (final-result (status HIU) (risk ?risk))
   =>
   (printout t "Result: HIU detected." crlf)
   (printout t "Risk Level: " ?risk crlf crlf)
)

; =========================
; EXPLANATIONS ONLY FOR HIU
; =========================

(defrule explain-q2
   (final-result (status HIU))
   (assessment (q2 ?x&:(>= ?x 2)))
   =>
   (printout t "Note: Prefers internet over social interaction." crlf)
)

(defrule explain-q4
   (final-result (status HIU))
   (assessment (q4 ?x&:(>= ?x 2)))
   =>
   (printout t "Note: Internet use affects health or habits." crlf)
)

(defrule explain-q5
   (final-result (status HIU))
   (assessment (q5 ?x&:(>= ?x 2)))
   =>
   (printout t "Note: Avoids other activities." crlf)
)

(defrule explain-q6
   (final-result (status HIU))
   (assessment (q6 ?x&:(>= ?x 2)))
   =>
   (printout t "Note: Failed to reduce usage." crlf)
)

(defrule explain-q7
   (final-result (status HIU))
   (assessment (q7 ?x&:(>= ?x 2)))
   =>
   (printout t "Note: Affects school or work." crlf)
)