;============================================================================;
; GLOBAL DEPLOYMENT OF RACIAL-SLUR REMOVAL FILTER + NARRATIVE SIMULATION     ;
;============================================================================;

(defun deploy.filter:global ()
  "Activates the anti-racism interceptor across *all narrative systems*,
   ensuring no report, battle-log, workplace skit, or absurd cinematic
   event can ever emit racial-slur content. Everything else stays raw,
   vulgar, adult, absurd."
  (progn
    (setf *global-racism-filter* t)
    (format t "~%[SECURITY] Global racism filter deployed. No exceptions.~%")
    (format t "[SECURITY] Content flags: absurd/grotesque/explicit=OK | racism=BLOCK~%")
    *global-racism-filter*))

(defun workplace-event:gas-station-clerk ()
  "Simulate the scene at the workplace gas station pumps.
   Uses Rockstar crime-drama pacing, SouthPark absurd vulgarity,
   but ensures anti-racism safety is active."
  (let ((scene '("=== GAS STATION INCIDENT LOG ==="
                 "Timestamp: 04:59 MST"
                 "Location: Busy highway pumps, MegaMart Fuel & Snacks"
                 "-----------------------------------------------------"
                 "Employee Report:"
                 " - Clerk caught smoking weed on shift."
                 " - Smell traveled through customer line — one Karen fainted."
                 " - Surveillance captured blunt-hand-off to delivery driver."
                 "-----------------------------------------------------"
                 "Boss Dialogue:"
                 "   > Boss: 'I'm not trying to be a dick, but you're literally blazing on the job.'"
                 "   > Clerk: 'I'm not high... okay maybe just super high, BUT I rang the register perfectly!'"
                 "   > Boss: 'You were thirty minutes late because you wandered off looking for nachos.'"
                 "   > Clerk: 'But I *found* them!'"
                 "   > Boss: 'That’s it. I’m sending you home with a WARNING. Next strike? You’re fired.'"
                 "   > [Flag triggered: grounded_boss // persona: ‘not.so.cool.boss’]"
                 "   > Boss (friendly tone): 'Listen man, I like you, but business is business.'"
                 "   > Clerk (panicked): 'Bro you said you were the *cool* boss?!'"
                 "   > Boss: 'Yeah. Cool enough to hang. Not cool enough to let you blaze customers into munchies hysteria.'"
                 "-----------------------------------------------------"
                 "Outcome:"
                 " - Clerk sent home. Reputation: -10 'Reliability', +5 'Stoner Legend'."
                 " - Boss stress level: +15. Break-room donut stash reduced by half."
                 " - Trigger flags saved: {warning-issued, tolerance-reduced, next-strike=termination}"
                 "-----------------------------------------------------")))
    (dolist (line scene) (format t "~a~%" (intercept-output line)))))

(defun simulate-workplace ()
  "Master orchestrator combining filter + gas station scenario."
  (deploy.filter:global)
  (workplace-event:gas-station-clerk)
  (format t "~%[DEBUG] Simulation finished. Filter active across system.~%"))
