;===================================
;=======Computer classifier=========
;======Todor Mihaylov==M24425=======
;===================================

	
;=====Functions - Ask Questions=====
(deffunction ask-question (?question $?allowed-values)
	(printout t crlf)
	(printout t ?question)
	(printout t crlf)
	(bind ?answer (read))
	(if (lexemep ?answer) then
		(bind ?answer (lowcase ?answer)))
	(while (not (member ?answer ?allowed-values)) do
		(printout t crlf)
		(printout t ?question)
		(bind ?answer (read))
		(if (lexemep ?answer) then
			(bind ?answer (lowcase ?answer))))
	?answer)
   
(deffunction ask-question-number-ans (?question)
	(printout t crlf)
	(printout t ?question)
	(printout t crlf)
	(bind ?answer (read))
	(while (not (numberp ?answer)) do
		(printout t crlf)
		(printout t ?question)
		(bind ?answer (read)))
	?answer)

(deffunction ask-question-integer-ans (?question)
	(printout t crlf)
	(printout t ?question)
	(printout t crlf)
	(bind ?answer (read))
	(while (not (integerp ?answer)) do
		(printout t crlf)
		(printout t ?question)
		(bind ?answer (read)))
	?answer)

(deffunction ask-question-integer-ans-in-bounds (?question ?lower-bound ?upper-bound)
	(bind ?answer
		  (ask-question-integer-ans ?question))
	(while (not (<= ?lower-bound ?answer ?upper-bound))
		(bind ?answer
			  (ask-question-integer-ans ?question)))
	?answer)

(deffunction yes-or-no-p (?question)
	(bind ?response (ask-question ?question yes no y n))
	(if (or (eq ?response yes) (eq ?response y)) then
		TRUE
	else
		FALSE))
		
;===== Fact rules========

;===Computer type
(defrule check-comp-type "Type: Laptop(L), Desktop(D), Rack(R)"
    =>
    (bind ?response
          (ask-question "What is the computer type: Laptop-(L), Desktop-(D), Rack-(R):"
                        L l D d R r))
    (if (or (eq ?response L) (eq ?response l)) then
        (assert (comptype Laptop))
    else
        (if (or (eq ?response D) (eq ?response d)) then
            (assert (comptype Desktop))
        else
            (assert (comptype Rack))))
    (assert (checked comptype)))

;===Processor frequency
(defrule check-processor-freq "processor frequency : Slow Normal Fast"
    =>
    (bind ?response
          (ask-question-integer-ans-in-bounds "What is the processor frequency? (between 0 MHZ and 10 000 MHz):"
                                              0 10000))
    (if (>= ?response 2800) then
        (assert (frequency High))
    else
        (if (>= ?response 1400) then
            (assert (frequency Medium))
        else
            (assert (frequency Low))))
    (assert (checked frequency)))
	
;===Processor count
(defrule check-processor-count "processor count : 1,1-3,4+"
    =>
    (bind ?response
          (ask-question-integer-ans-in-bounds "What is the processors count? 1-128:"
                                              1 128))
    (if (>= ?response 4) then
        (assert (proccount 0-1))
    else
        (if (>= ?response 2) then
            (assert (proccount 2-3))
        else
            (assert (proccount 4+))))
    (assert (checked proccount)))
	
;===RAM
(defrule check-ram "ram : Low Medium High"
    =>
    (bind ?response
          (ask-question-integer-ans-in-bounds "What is the operation memory(RAM)? 0-128000 MB:"
                                              1 128000))
    (if (>= ?response 8000) then
        (assert (ram High))
    else
        (if (>= ?response 2000) then
            (assert (ram Medium))			
			else
				(assert (ram Low))))
    (assert (checked ram)))	
	
;==HDD Capacity	
(defrule check-hddcapacity "HDD Capacity : Low Medium High"
    =>
    (bind ?response
          (ask-question-integer-ans-in-bounds "What is the HDD Capacity? 0-16000 GB :"
                                              1 16000))
    (if (>= ?response 600) then
        (assert (hddcapacity High))
    else
        (if (>= ?response 250) then
            (assert (hddcapacity Medium))			
			else
				(assert (hddcapacity Low))))
    (assert (checked hddcapacity)))	
	
;===HDD Speed	
(defrule check-hddspeed "HDD speed: Slow(s), Medium(m), Fast(f)"
    =>
    (bind ?response
          (ask-question "What is the HDD speed: Slow(s), Medium(m), Fast(f):"
                        s S m M f F ))
   (if (or (eq ?response S) (eq ?response s)) then
        (assert (hddspeed Slow))
    else
       (if (or (eq ?response M) (eq ?response m)) then
             (assert (hddspeed Medium))
        else
            (assert (hddspeed Fast))))
    (assert (checked hddspeed)))

;===Video power
(defrule check-videopower "video power: Low(l), Medium(m), High(h)"
    =>
    (bind ?response
          (ask-question "What is the video power: Low(l), Medium(m), High(h)"
                        l L m M h H ))
   (if (or (eq ?response L) (eq ?response l)) then
        (assert (videopower Low))
    else
       (if (or (eq ?response M) (eq ?response m)) then
             (assert (videopower Medium))
        else
            (assert (videopower High))))
    (assert (checked videopower)))


;===Monitor size
(defrule check-monitor "monitor size: Small, Medium, Large"
    =>
    (bind ?response
          (ask-question-integer-ans-in-bounds "What is the Monitor size? 1-85 inches:"
                                              1 85))
    (if (>= ?response 21) then
        (assert (monitor Large))
    else
        (if (>= ?response 15) then
            (assert (monitor Medium))
        else
            (assert (monitor Small))))
    (assert (checked monitor)))
	
;===Cooling
(defrule check-cooling "computer cooling: Normal(n), Good(g), Extra(e)"
    =>
    (bind ?response
          (ask-question "What is the computer cooling: Normal(n), Good(g), Extra(e)"
                        n N g G e E ))
   (if (or (eq ?response N) (eq ?response n)) then
        (assert (cooling Normal))
    else
       (if (or (eq ?response G) (eq ?response g)) then
             (assert (cooling Good))
        else
            (assert (cooling Extra))))
    (assert (checked cooling)))


;==EXTRA

;===USB
(defrule check-for-usb "USB : yes/no"
    =>
    (if (yes-or-no-p "Does the computer have an USB? (yes/no)") then
        (assert (extra USB)))
    (assert (checked usb)))
	
;===HDMI
(defrule check-for-hdmi "HDMI : yes/no"
    =>
    (if (yes-or-no-p "Does the computer have a HDMI? (yes/no)") then
        (assert (extra HDMI)))
    (assert (checked hdmi)))

;===VGA
(defrule check-for-vga "VGA : yes/no"
    =>
    (if (yes-or-no-p "Does the computer have a VGA? (yes/no)") then
        (assert (extra VGA)))
    (assert (checked vga)))
	
;===dvdrom
(defrule check-for-dvdrom "DVD-ROM : yes/no"
    =>
    (if (yes-or-no-p "Does the computer have a CD/DVD-R/RW? (yes/no)") then
        (assert (extra DVD-CD-R-RW)))
    (assert (checked dvdrom)))	

	
;===COMPUTER TYPE EVALUATION RULES
(defrule all-checked "Assert that all specifications are checked"
	(checked comptype)
	(checked frequency)
	(checked proccount)
	(checked ram)
	(checked hddcapacity)
	(checked hddspeed)
	(checked videopower)
	(checked monitor)
	(checked cooling)
	(checked usb)
	(checked vga)
	(checked hdmi)
	(checked dvdrom)
	=>
	(printout t "All checked" crlf)
	(assert (all-checked)))



;==evaluate for docs
(defrule evaluate-for-docs-and-browsing
	(or (comptype Laptop)
		(comptype Desktop)
		(comptype Rack))	
		
	(or (frequency Low)
		(frequency  Medium)
		(frequency  High))	
		
	(or (proccount 0-1)
		(proccount 2-3)
		(proccount 4+))	
		
	(or (ram Low)
		(ram Medium)
		(ram High))	
		
	(or (hddcapacity Low)
		(hddcapacity Medium)
		(hddcapacity High))	
		
	(or (hddspeed Slow)
		(hddspeed Medium)
		(hddspeed Fast))	
		
	(or (videopower Low)
		(videopower Medium)
		(videopower High))	
		
	(or (monitor Small)
		(monitor Medium)
		(monitor Large))	
		
	(or (cooling Normal)
		(cooling Good)
		(cooling Extra))	
	(extra usb)
	(extra vga)
	(extra hdmi)
	(extra dvdrom)
    =>
    (printout t "The computer is suitable for Docs and Internet" crlf)
    (assert (suitable-for DocsAndInternet)))



















		