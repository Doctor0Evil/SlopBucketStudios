;; Roach Boss Engage Speech - SlopBucket Studios Adult Chaotic Narrative
(defun engage-roach-boss-speech (trigger)
  (format t "~%[Roach Boss, slick in a grease-stained tracksuit, cracks a bottle over a rusty gear. A cologne cloud of citronella and Olde English surrounds him. His antennae twitch at the player.]~%")
  (case trigger
    (:user-interaction
      (format t "~%ROACH BOSS: Who dares disturb my trash throne, eh? I was mid-siesta! If it's scrap you want, you'll have to dance the 'Bin-Dusta Shuffle.' Or bribe me with a six-pack—your call, freakshow.~
        ~%You see that limp worker drooling in the sorting pit? Don't step on 'im, that's Carl. He's my best friend. Only one who listens. Sometimes he thinks *he's* the boss—HA!~
        ~%Now hand over the bottle, new meat, or go wrestle the compactor. Maybe you'll get lucky and only lose a limb.~
        ~%...Or stay for story time. Did I ever tell you about the time I lost my nut-wrinkles during the Great Liberty Freak Waster Assault of '92? Oh, the itching—"
      ))
    (:fail-roach-exe
      (format t "~%[Roach Boss spazzes, eyes rolling]"~
        "~%ROACH BOSS: Aw hell, not again... system’s gone sideways—assemble the rest, shift's over! I need my crutch and a double shot of ‘Bugshine.’ No sleeping in the bin unless you wear your helmet!~
        ~%Carl, wake your drunk ass up! We got outsiders and not enough RAID to go ‘round!~
        "
      ))
    (t (format t "~%ROACH BOSS: If you’re not working, you’re part of the waste-stream. Choose wisely!~%")))
