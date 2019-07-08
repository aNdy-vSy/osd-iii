;
; ------------------------------------------------------------
;
;   Cosine - Old School Demo 3

;   for the DBF Halloween Challenge 2018
;
;   http://www.dbfinteractive.com/forum/index.php?topic=6784.0
;
;   Released 2018-10-31
;
;   Code by aNdy/AL/Cosine
;   GFX by aNdy/AL/Cosine
;   Music by aNdy/AL/Cosine (samples from ST-xx packs)
;
;   www.cosine.org.uk
;
;   The source is provided as a learning tool for others and,
;   yes, some bits could be reduced in size using loops, etc.,
;   but since this is for beginners to follow also, it
;   has been coded and commented to be as readable and 
;   friendly as possible.
;
;   All of the graphics are provided in their original bitmap
;   format so you can see how they all slot together.
;
;   The main music is in OGG format when loaded in the demo, but the
;   original MOD that was written especially for this demo is
;   also included in the 'extras' folder so you can see how that
;   was done too. It was written in OpenMPT, but should load
;   into any tracker that supports MOD format.
;
; ------------------------------------------------------------
;
;
; >>>>>>>>>> NOTE TO LINUX USERS <<<<<<<<<<
;
; Before compile, uncomment the 'Import C' line below and uncheck
; 'Use Compiler' in 'Compiler > Compiler Options' menu.
;
;
; ImportC "-no-pie" : EndImport
;
;
;
;- INITIALISE ENVIRONMENT ------------------------------------


InitKeyboard()            ; we will be testing the keyboard for various keypresses
InitSprite()              ; we will be using sprites, plus needs to be called for screen use anyway
InitSound()               ; we will be loading and using some sound
UseOGGSoundDecoder()      ; we will need to use the ogg decoder to load and play the ogg tune file


;- CREATE VARIABLES ------------------------------------------

; these variables are used in various places throughout, so are set here as 'global'
; you will see them used in various procedures so should see what they do further on!
;
; please note, it's a bit lame using such long variable names, but by using these long
; words, anybody reading has half a chance of understanding what's going on and I am
; sharing the source!  read the purebasic manual for info on better naming conventions
; for variable use!  </excuse>

#XRES = 800 ; window 'x' size
#YRES = 600 ; window 'y' size

; music volume at 100%, demopart set to 1, subpart set to 1 and the
; silhoueete flash set at 0.  scanlines are switched on (1) at the start!

Global vol = 80, demopart = 1, subpart = 1, flash = 0, scanlines = 1, c64load = 1

Global c64sprite = 0, c64spritedir = 0, c64spritex = 150, pennywise = 0

; here are all the initial x-y sreen positions of various images/sprites,
; as well as their timers if they have one at first run

Global moonrisey = 750 ; y position of the big moon
Global bloody = -1800  ; y position of the big blood drio
Global transparency = 0 ; initial transparency setting
Global witchx = 850, witchy = 100, witchzoomx = 68, witchzoomy = 62 ; the witch positions
Global zombiex = -400 ; zombie x position
Global batx = 650, baty = 325, batpath = 1 ; the bats
Global owlx = 850, owly = 400 ; the owl
Global cloud1x = -674, cloud2x = 350 ; some clouds
Global scroll3y = 330, scrolldir = 1 ; 

; all silhouette sprites are not displayed at the start so their values are 0!

Global lights = 0 : spider = 0 : witch = 0 : bats = 0 : owl = 0 : zombie = 0 : ghost = 0 : hand = 0

; all the silhouette sprites can be switched on or off at the same time with a certain keypress!

Global allsprites = 0

Global blooddrip1y = -50,  blooddrip2y = -50, blooddrip3y = -50; blood drips screen position values
Global blooddriptimer=ElapsedMilliseconds()    ; this variable controls the frequency of the blood drips

Global shootstar = 0 : shootstary = -100 : shootstarx = 0 ; shooting star screen position values
Global shootstartimer=ElapsedMilliseconds()    ; this variable controls the frequency of the shooting star

Global ghostdisplay = 0 : ghosty = 650 : ghostx = 0 ; ghost screen position values
Global ghosttimer=ElapsedMilliseconds()    ; this variable controls the frequency of the ghost

Global Dim a_SPIDERANIM(8)                     ; array holding spider animation frames
Global spideranimtime=ElapsedMilliseconds()    ; this variable controls spider animation time between frames

Global Dim a_HANDANIM(8)                     ; array holding spider animation frames
Global handanimtime=ElapsedMilliseconds()    ; this variable controls spider animation time between frames

Global Dim a_WITCHANIM(3)                     ; array holding witch animation frames
Global witchanimtime=ElapsedMilliseconds()    ; this variable controls witch animation time between frames

Global Dim a_WINDOWANIM(8)                     ; array holding window glow animation frames
Global windowanimtime=ElapsedMilliseconds()    ; this variable controls window animation time between frames

Global Dim a_BATANIM(3)                     ; array holding bats animation frames
Global batanimtime=ElapsedMilliseconds()    ; this variable controls bats animation time between frames

Global Dim a_GHOSTANIM(5)                     ; array holding ghost animation frames
Global ghostanimtime=ElapsedMilliseconds()    ; this variable controls ghost animation time between frames

Global Dim a_ZOMBIEANIM(6)                     ; array holding zombie animation frames
Global zombieanimtime=ElapsedMilliseconds()    ; this variable controls zombie animation time between frames

Global owlanimtime=ElapsedMilliseconds()        ; this variable controls owl animation time between frames

Global cloud1scrolltime=ElapsedMilliseconds()    ; this variable controls cloud 1 scroll speed on screen
Global cloud2scrolltime=ElapsedMilliseconds()    ; this variable controls cloud 2 scroll speed on screen

Global starfieldscrolltime=ElapsedMilliseconds()    ; this variable controls starfield scroll speed on screen

Global moonscrolltime=ElapsedMilliseconds()    ; this variable controls moonrise scroll speed on screen


; the following arrays and variables are created for the c64 graphics for the hidden part!

Global c64displaytime=ElapsedMilliseconds()    ; this variable controls time between c64 screens
Global C64spriteanimtime=ElapsedMilliseconds() ; this variable controls witch animation time between frames

Global Dim a_C64WITCHANIMR(3)                     ; array holding c64 witch right animation frames
Global Dim a_C64WITCHANIML(3)                     ; array holding c64 witch left animation frames
Global Dim a_C64GHOSTANIMR(5)                     ; array holding c64 ghost right animation frames
Global Dim a_C64GHOSTANIML(5)                     ; array holding c64 ghost left animation frames
Global Dim a_C64OWLANIMR(4)                       ; array holding c64 owl right animation frames
Global Dim a_C64OWLANIML(4)                       ; array holding c64 owl left animation frames
Global Dim a_C64ZOMBIEANIMR(3)                    ; array holding c64 zombie right animation frames
Global Dim a_C64ZOMBIEANIML(3)                    ; array holding c64 zombie left animation frames


; the following string variables contains the text to use in the scrolling messages
; it starts as t$ and then each line adds more to the t$
;
; t1$ is the main DYCP scoller message, t2$ is the vertical greetings scroller
; t3$ is the scrolling message in the hidden part
;
; in the messages below, the '=' displays as a pumpkin, the '@' as the cosine symbol
; the '#' as left bloody handprint, '$' as a right bloody handprint and the lowercase
; parts of my handle (aNdy/AL/Cosine) made using the symbols '& ( )'

Global sco1=0 : tptr1=1 : sco2=0 : tptr2=1 : sco3=0 : tptr3=1 ; variables controlling the scrolling messages

t1$="                               "
t1$=t1$+"WOOOOOOOOO! = ON THIS GHOULISH NIGHT...  ...WAIT A SECOND, IT'S A BIT DARK...    LETS HAVE A MOON...     "
t1$=t1$+"SHOULD MAKE THINGS EASIER TO SEE  =  ON THIS SPOOK-TACULAR NIGHT, COSINE @ PRESENTS  # OLD SCHOOL DEMO III $ "
t1$=t1$+"= CODED FOR THE DBF HALLOWEEN 2018 CHALLENGE = CODE, PIXELS AND MUSIC BY &N()/AL/COSINE = BEFORE YOU READ ANY FURTHER, "
t1$=t1$+"THIS DEMO HAS SOME INTERACTIVITY = TRY PRESSING SOME KEYS TO SWITCH ON AND OFF SOME SPRITE ANIMATIONS = WHICH "
t1$=t1$+"KEYS? YOU'LL HAVE TO DISCOVER FOR YOURSELF = THIS DEMO IS THE LATEST IN MY 'OLD SCHOOL' SERIES WHICH HAVE BEEN "
t1$=t1$+"CODED TO LEARN NEW THINGS IN PUREBASIC = I GOT TO THINKING, SINCE THIS SERIES IS SUPPOSED TO BE 'OLD SCHOOL' "
t1$=t1$+"IN STYLE, SOME SCANLINE EMULATION MIGHT BE IN ORDER SO EVERTHING LOOKS LIKE IT'S BEING DISPLAYED ON A CRT SCREEN = "
t1$=t1$+"HIT TAB TO TOGGLE SCANLINES = WHAT IS NEW FOR ME IN THIS DEMO? ONCE ALL THE SPRITES ARE ON, THIS IS "
t1$=t1$+"THE MOST SPRITES I'VE HANDLED AND ALMOST EVERYTHING IS BEING TIMER CONTROLLED INSTEAD OF USING FOR/NEXT LOOPS "
t1$=t1$+"OR DISPLAYING SPRITES OFF SCREEN WITH RIDICULOUSLY LARGE X-Y POSITIONS AND MOVING BY MULTIPLE PIXELS = ALSO NEW "
t1$=t1$+"IS MY USE OF TRANSPARENCIES TO PRODUCE THE SILHOUETTE EFFECTS = THIS IS THE FIRST TIME I'VE DONE A VERTICAL "
t1$=t1$+"SCROLLER AND IS ALSO THE FIRST TIME I'VE DONE KEYBOARD CONTROL, AS MENTIONED EARLIER, TO CONTROL THE SPRITES = "
t1$=t1$+"THE MUSIC IS BRAND NEW, WRITTEN IN 4 CHANNELS IN PROTRACKER FORMAT USING THE ORIGINAL SAMPLES FROM THE AMIGA "
t1$=t1$+"ST-XX SAMPLE DISKS = I'VE ATTEMPTED TO COMPOSE SOMETHING THAT MIMICS MUSIC FROM AN AMIGA DEMO CIRCA 1988, "
t1$=t1$+"ALMOST AS IF IT WAS ONE OF THE FIRST MODULES WRITTEN USING AN EARLY VERSION OF SOUNDTRACKER = "
t1$=t1$+"THOSE WITH SOME MUSICAL KNOWLEDGE MIGHT NOTICE THAT SOME OF THE MUSIC IS WRITTEN IN 7/8 FOR EXTRA HALLOWEEN "
t1$=t1$+"EDGINESS AND, YES, THAT MAY BE A RIFF FROM TUBULAR BELLS AS USED IN THE 1973 FILM THE EXORCIST AND, ALSO YES, "
t1$=t1$+"YOU MAY HEAR SOME LOW QUALITY 8BIT SAMPLES, JUST LIKE IN THE EARLY AMIGA DAYS, FROM FAMOUS HORROR FILMS OF "
t1$=t1$+"YESTERYEAR = SOME OF THE SPRITES ARE FROM A C64 HALLOWEEN DEMO CALLED 'SABBAT' BY ARKANIX LABS, WHICH I AM ALSO A "
t1$=t1$+"MEMBER OF = THE ORIGINAL C64 SPRITES WERE PIXELLED BY ME IN HIRES AND HAVE BEEN CONVERTED HERE WITH SOME EXTRA "
t1$=t1$+"DEFINITION TO HOLD UP ON A LARGER SCREEN RESOLUTION = AS WITH THE OTHER DEMOS IN THE 'OLD SCHOOL' SERIES, THE "
t1$=t1$+"SOURCE SHOULD HAVE BEEN INCLUDED WITH THE DOWNLOAD OF THIS DEMO, ALONG WITH ALL THE GRAPHIC AND SOUND FILES IN "
t1$=t1$+"THEIR ORIGINAL FORMATS SO YOU CAN SEE HOW IT ALL SLOTS TOGETHER = "
t1$=t1$+"THANKS GO TO MOLOCH, WARLOCK, FUZZ AND T.M.R FOR TESTING THIS DEMONSTRATION = IF YOU HAVE ANY SPARE TIME, "
t1$=t1$+"PLEASE VISIT THE COSINE AND ARKANIX LABS WEBSITES ->    WWW.COSINE.ORG.UK  =  WWW.ARKANIXLABS.COM  =  "
t1$=t1$+"UNTIL NEXT TIME, THIS IS &N()/AL/COSINE SIGNING OFF...  HAVE A FANG-TASTIC HALLOWEEN!  =  WHOOPS! I FORGOT TO "
t1$=t1$+"MENTION... THERE MAY BE A HIDDEN PART TO THIS DEMO... .. .                     "
t1$=t1$+"                                       "

t2$="                                                                                             "
t2$=t2$+"GHOSTLY GREETINGS GO OUT TO GROUPS       ABSENCE        ABYSS CONNECTION        ARKANIX LABS        "
t2$=t2$+"ARTSTATE        ATE BIT        ATLANTIS        BOOZE DESIGN        CAMELOT        "
t2$=t2$+"CENSOR DESIGN        CHORUS        CHROME        CNCD        CPU        CRESCENT        "
t2$=t2$+"COVERT BITOPS        DEFENCE FORCE        DEKADENCE        DESIRE        DAC        DMAGIC        DUALCREW        "
t2$=t2$+"EXCLUSIVE ON        FAIRLIGHT        F4CG        FIRE        FLAT 3        FOCUS        FRENCH TOUCH        "
t2$=t2$+"GENESIS PROJECT        GHEYMAID INC.        HITMEN        HOKUTO FORCE        LEGION OF DOOM        "
t2$=t2$+"LEVEL64        MANIACS OF NOISE        MAYDAY        MEANTEAM        METALVOLTE        NONAME        "
t2$=t2$+"NOSTALGIA        NUANCE        OFFENCE        ONSLAUGHT        ORB        OXYRON        PADUA        "
t2$=t2$+"PERFORMERS        PLUSH        PROFESSIONAL PROTECTION CRACKING SERVICE        PSYTRONIK        "
t2$=t2$+"REPTILIA        RESOURCE        RGCD        SECURE        SHAPE        SIDE B        SINGULAR         "
t2$=t2$+"SLASH        SLIPSTREAM        SUCCESS AND TRC        STYLE        SUICYCO INDUSTRIES        "
t2$=t2$+"TAQUART        TEMPEST        TEK        TRIAD        TRSI        VIRUZ        VISION        WOW        "
t2$=t2$+"WRATH        XENON        EVERYONE ON THE DBF FORUM          "
t2$=t2$+"AND GREETINGS TO INDIVIDUALS           MOLOCH        WARLOCK        FUZZ        "
t2$=t2$+"REDBACK        TMR        ODIE        AND ALL OTHERS IN BOTH COSINE AND ARKANIX LABS          "
t2$=t2$+"                                              "

t3$="                                   "
t3$=t3$+"WELCOME TO A DEMO IN A DEMO!   THIS IS A LITTLE TRIBUTE TO A 2013 C64 HALLOWEEN DEMO CALLED 'SABBAT' RELEASED BY "
t3$=t3$+"ARKANIX LABS.    SABBAT HAD A HIDDEN PART FULL OF SILHOUETTE IMAGES.    IN A TURNAROUND, THIS OLD SCHOOL DEMO "
t3$=t3$+"HAS THE MAIN PART CONSISTING OF SILHOUETTES INSPIRED BY SABBAT BUT THE HIDDEN PART AS A SIMPLIFIED VERSION OF "
t3$=t3$+"THE SABBAT DEMO ITSELF TO SPREAD FURTHER HALLOWEEN LOVE!    THE MONITOR BEZEL HERE WAS PIXELLED  "
t3$=t3$+"BY T.M.R USING DELUXE PAINT IV AGA ON THE AMIGA QUITE A LONG TIME AGO AND HE RECENTLY CONVERTED THE IFF FILE "
t3$=t3$+"TO USE IN THE YOUTUBE VIDEOS THAT ACCOMPANY HIS BLOG.    ALL THE OTHER C64 GRAPHICS HERE WERE PIXELLED "
t3$=t3$+"BY ME FOR SABBAT.    THE MUSIC PLAYING NOW WAS COMPOSED BY ME SPECIFICALLY FOR SABBAT AND IS A "
t3$=t3$+"RECORDING OF THE ORIGINAL SID TUNE FROM MY ACTUAL 8580 EQUIPPED C64C PLAYING THROUGH A DOLBY AMP.    " 
t3$=t3$+"CREDIT FOR THE DESIGN OF SABBAT GOES TO MOLOCH/TRIAD WHO CODED SABBAT ORIGINALLY, ALONG WITH FUZZ OF RETRO64.   "
t3$=t3$+"WARLOCK PIXELLED THE PUMPKIN AND KNIFE BITMAP.    SEARCH FOR SABBAT ON CSDB TO SEE THE ORIGINAL DEMO.    "
t3$=t3$+"PRESS SPACE TO SEE THE ORIGINAL SABBAT DEMO HIDDEN SILHOUETTE HIRES BITMAP THAT INSPIRED THIS 'OLD SCHOOL DEMO III'"
t3$=t3$+"                                  "


;- PROCEDURES ------------------------------------------------

; here are various procedures that loads images/sounds and draws/animates various parts of the screen
; as well as checking for key presses.  the procedure names are fairly self explanatory!?


Procedure OpenMainWindow()
  
  ; this procedure opens a windows with a resolution of the XRES and #YRES variable defined
  ; earlier.  give the window a title, centre it and give it a small title bar with no
  ; taskbar entry.
  ;
  ; next, open a windowed screen on the window we've just opened so we can use sprite commmands
  ; and fast graphical effects.  this screen has the same dimensions as the window.
  
  OpenWindow(0,0,0,#XRES,#YRES,"Cosine - Old School Demo III - DBF Halloween 2018 Challenge - 'ESC' to Exit", #PB_Window_ScreenCentered | #PB_Window_Tool)
  OpenWindowedScreen(WindowID(0),0,0,#XRES,#YRES)
  
EndProcedure

Procedure DisplayLoader()
  
  ; there are quite a few images and sounds to load, so load up a 'loading' message
  ; screen first and display it straight away.  that way, the person viewing knows
  ; that something is happening!  a blank or empty window for a few seconds might
  ; look like a crash otherwise!
  
  Global sp_LOADING = LoadSprite(#PB_Any, "gfx/loading.bmp") ; load the loading image
  ClearScreen(0)                                              ; clear the screen to black
  DisplaySprite(sp_LOADING,0,0)                               ; draw the 'loading' image
  FlipBuffers()                                               ; show the image 
  
EndProcedure  

Procedure LoadAssets()
  
  ; here, we load the various images for the demo.  the images are loaded as sprites so we
  ; can make them move around and display nice and quickly on the screen.  by using
  ; sprites, we can also display them using the transparent function to make use of
  ; the transparent and intensity functions.
  
  ; to explain my naming convention below... I use the prefix 'sp_' at the front of the variable
  ; to indicate a sprite so 'sp_MOON' is a variable that holds the sprite image of the moon
  ; in the background
  
  Global sp_SCANLINES = LoadSprite(#PB_Any, "gfx/scanlines.bmp")          ; load some scanlines
  
  Global sp_PUMPKIN = LoadSprite(#PB_Any, "gfx/flash/pumpkin.bmp")        ; load a pumpkin
  Global sp_MASK = LoadSprite(#PB_Any, "gfx/flash/jason.bmp")             ; load a vorhees!
  Global sp_SKULL = LoadSprite(#PB_Any, "gfx/flash/skull.bmp")            ; load a skull
  Global sp_CLEAVER = LoadSprite(#PB_Any, "gfx/flash/cleaver.bmp")        ; load a cleaver
  Global sp_GHOSTY = LoadSprite(#PB_Any, "gfx/flash/ghosty.bmp")          ; load a ghost 
  
  Global sp_BLOODDRIP = LoadSprite(#PB_Any, "gfx/blooddrip.bmp")          ; load some big blood drips
  Global sp_BLOODSMALL = LoadSprite(#PB_Any, "gfx/blooddripsmall.bmp")    ; load some big blood drips
  Global sp_COSPRES = LoadSprite(#PB_Any, "gfx/cospres.bmp")              ; load cosine pres title
  Global sp_COSURL = LoadSprite(#PB_Any, "gfx/cosurl.bmp")                ; load cosine web  
  Global sp_DBFCHALL = LoadSprite(#PB_Any, "gfx/dbfchall.bmp")            ; load dbf title
  
  Global sp_GRAVEYARD = LoadSprite(#PB_Any, "gfx/graveyard.bmp")          ; load the background
  
  Global sp_MOON = LoadSprite(#PB_Any, "gfx/moon.bmp")                    ; load a moon
  
  Global sp_SHOOTSTAR = LoadSprite(#PB_Any, "gfx/shootstar.bmp")          ; load a shooting star
  
  Global sp_CLOUD1 = LoadSprite(#PB_Any, "gfx/cloud1.bmp")                ; load a cloud
  Global sp_CLOUD2 = LoadSprite(#PB_Any, "gfx/cloud2.bmp")                ; load another cloud
  
  Global sp_OWL = LoadSprite(#PB_Any, "gfx/owlframes.bmp")                ; load the agony owl image
  
  a_SPIDERANIM(0) = LoadSprite(#PB_Any, "gfx/spiderframes/spider1.bmp")    ; load spider animation frames
  a_SPIDERANIM(1) = LoadSprite(#PB_Any, "gfx/spiderframes/spider2.bmp")    ; into diffrent parts of the array
  a_SPIDERANIM(2) = LoadSprite(#PB_Any, "gfx/spiderframes/spider3.bmp")    ; that was dimensioned earlier
  a_SPIDERANIM(3) = LoadSprite(#PB_Any, "gfx/spiderframes/spider4.bmp") 
  a_SPIDERANIM(4) = LoadSprite(#PB_Any, "gfx/spiderframes/spider5.bmp") 
  a_SPIDERANIM(5) = LoadSprite(#PB_Any, "gfx/spiderframes/spider6.bmp")
  a_SPIDERANIM(6) = LoadSprite(#PB_Any, "gfx/spiderframes/spider7.bmp")
  a_SPIDERANIM(7) = LoadSprite(#PB_Any, "gfx/spiderframes/spider8.bmp")
  a_SPIDERANIM(8) = LoadSprite(#PB_Any, "gfx/spiderframes/spider9.bmp")
  
  a_WITCHANIM(0) = LoadSprite(#PB_Any, "gfx/witchframes/witch1.bmp")       ; load witch animation frames
  a_WITCHANIM(1) = LoadSprite(#PB_Any, "gfx/witchframes/witch2.bmp")       ; into diffrent parts of the array
  a_WITCHANIM(2) = LoadSprite(#PB_Any, "gfx/witchframes/witch3.bmp")       ; that was dimensioned earlier
  a_WITCHANIM(3) = LoadSprite(#PB_Any, "gfx/witchframes/witch4.bmp")
  
  a_BATANIM(0) = LoadSprite(#PB_Any, "gfx/batsframes/bats1.bmp")           ; load bat animation frames into
  a_BATANIM(1) = LoadSprite(#PB_Any, "gfx/batsframes/bats2.bmp")           ; the bat array
  a_BATANIM(2) = LoadSprite(#PB_Any, "gfx/batsframes/bats3.bmp") 
  a_BATANIM(3) = LoadSprite(#PB_Any, "gfx/batsframes/bats4.bmp")
  
  a_WINDOWANIM(0) = LoadSprite(#PB_Any, "gfx/windowframes/window1.bmp")    ; load window animation frames
  a_WINDOWANIM(1) = LoadSprite(#PB_Any, "gfx/windowframes/window2.bmp")    ; just like the spider, bats, etc
  a_WINDOWANIM(2) = LoadSprite(#PB_Any, "gfx/windowframes/window3.bmp")    ; above!
  a_WINDOWANIM(3) = LoadSprite(#PB_Any, "gfx/windowframes/window4.bmp")
  a_WINDOWANIM(4) = LoadSprite(#PB_Any, "gfx/windowframes/window5.bmp")
  a_WINDOWANIM(5) = LoadSprite(#PB_Any, "gfx/windowframes/window4.bmp")
  a_WINDOWANIM(6) = LoadSprite(#PB_Any, "gfx/windowframes/window3.bmp")
  a_WINDOWANIM(7) = LoadSprite(#PB_Any, "gfx/windowframes/window2.bmp")
  
  a_ZOMBIEANIM(0) = LoadSprite(#PB_Any, "gfx/zombieframes/zombie1.bmp")    ; load zombie animation frames  
  a_ZOMBIEANIM(1) = LoadSprite(#PB_Any, "gfx/zombieframes/zombie2.bmp")
  a_ZOMBIEANIM(2) = LoadSprite(#PB_Any, "gfx/zombieframes/zombie3.bmp")
  a_ZOMBIEANIM(3) = LoadSprite(#PB_Any, "gfx/zombieframes/zombie4.bmp")
  a_ZOMBIEANIM(4) = LoadSprite(#PB_Any, "gfx/zombieframes/zombie5.bmp")
  a_ZOMBIEANIM(5) = LoadSprite(#PB_Any, "gfx/zombieframes/zombie5.bmp")
  a_ZOMBIEANIM(6) = LoadSprite(#PB_Any, "gfx/zombieframes/zombie6.bmp")
  
  a_GHOSTANIM(0) = LoadSprite(#PB_Any, "gfx/ghostframes/ghost1.bmp")       ; load ghost animation frames  
  a_GHOSTANIM(1) = LoadSprite(#PB_Any, "gfx/ghostframes/ghost2.bmp") 
  a_GHOSTANIM(2) = LoadSprite(#PB_Any, "gfx/ghostframes/ghost3.bmp") 
  a_GHOSTANIM(3) = LoadSprite(#PB_Any, "gfx/ghostframes/ghost4.bmp") 
  a_GHOSTANIM(4) = LoadSprite(#PB_Any, "gfx/ghostframes/ghost5.bmp") 
  a_GHOSTANIM(5) = LoadSprite(#PB_Any, "gfx/ghostframes/ghost6.bmp") 
  
  a_HANDANIM(0) = LoadSprite(#PB_Any, "gfx/handframes/hand1.bmp")          ; load hand animation frames
  a_HANDANIM(1) = LoadSprite(#PB_Any, "gfx/handframes/hand2.bmp")  
  a_HANDANIM(2) = LoadSprite(#PB_Any, "gfx/handframes/hand3.bmp")
  a_HANDANIM(3) = LoadSprite(#PB_Any, "gfx/handframes/hand4.bmp")  
  a_HANDANIM(4) = LoadSprite(#PB_Any, "gfx/handframes/hand5.bmp")  
  a_HANDANIM(5) = LoadSprite(#PB_Any, "gfx/handframes/hand4.bmp")  
  a_HANDANIM(6) = LoadSprite(#PB_Any, "gfx/handframes/hand3.bmp")
  a_HANDANIM(7) = LoadSprite(#PB_Any, "gfx/handframes/hand2.bmp")  
  
  
  ; this next section creates the array and varibale that loads and stores the blooddrip
  ; font used in the scrolling messages
  
  Global Dim a_GFXFONT(60) ; array holding scroll font, 60 characters available
  
  Global i_FONT = LoadImage (#PB_Any, "gfx\bloodfont.bmp") ; load the blood font to cut up into characters
  
  ; now draw the loaded image onto the screen, then 'cut' it into each character and store in the array
  
  StartDrawing (ScreenOutput())
  DrawImage (ImageID(i_FONT),0,0)
  StopDrawing()  
  
  xp=0
  yp=0
  
  For loop = 1 To 60
    a_GFXFONT(loop) = GrabSprite(#PB_Any,xp,yp,31,31)  
    xp=xp+32
    If xp>=320
      yp=yp+33
      xp=0
    EndIf
  Next
  
  ClearScreen(0) ; once the font is cut up, clear the screen - we don't need the image anymore
  
  
  Global sp_LEFTFADE = LoadSprite(#PB_Any, "gfx/leftfade.bmp")   ; load a lefthand fade
  Global sp_RIGHTFADE = LoadSprite(#PB_Any, "gfx/rightfade.bmp") ; load a righthand fade

  
  ; finally, load the music and sounds
  
  Global m_CHOON = LoadSound(#PB_Any, "sfx/tubularween.ogg")           ; load the main music
  Global s_GROAN = LoadSound(#PB_Any, "sfx/zombiegroan.ogg")           ; load a zombie groan sound
  Global s_CACKLE = LoadSound(#PB_Any, "sfx/witchcackle.ogg")          ; load a witch cackle sound
  Global s_GHOST = LoadSound(#PB_Any, "sfx/ghost.ogg")                 ; load a ghost howl sound    
  Global s_LAUGH = LoadSound(#PB_Any, "sfx/rentaghost.ogg")            ; old memories for me!
  
  ; in this next section, a few c64 assets are loaded in case the user activates the hidden
  ; part of the demo.  the rest of the assets are loaded later if the hidden part is activated
  ; with the procedure 'loadc64assets' being called by demopart = 4 in the main program loop
  
  Global sp_MONITOR = LoadSprite(#PB_Any, "gfx/hidden/monitor.bmp")       ; load a C= monitor
  Global sp_BORDMASK = LoadSprite(#PB_Any, "gfx/hidden/bordermask.bmp")   ; load a C= border  
  Global sp_C64BOOT = LoadSprite(#PB_Any, "gfx/hidden/boot.bmp")          ; load c64 boot screen
  Global sp_SCANLINESSMALL = LoadSprite(#PB_Any, "gfx/hidden/scanlinessmall.bmp") ; load small scanlines
  
EndProcedure  

Procedure LoadC64Assets()

 ; the following images are loaded for the hidden part.  they are all bitmaps and
 ; sprites that were pixelled for a c64 demo called sabbat by me!
 ;
 ; these assets are only loaded if the hidden section is activated by the user.
 ; they are loaded while the hidden part displays the c64 boot screen.
    
  Global sp_SABBATLOGO = LoadSprite(#PB_Any, "gfx/hidden/sabbatlogo.bmp") ; load sabbat logo
  Global sp_CLOWN = LoadSprite(#PB_Any, "gfx/hidden/clown.bmp")           ; load a pennywise
  Global sp_CLOWNG = LoadSprite(#PB_Any, "gfx/hidden/clowng.bmp")         ; load a grey pennywise  
  Global sp_ARKLOGO = LoadSprite(#PB_Any, "gfx/hidden/allogo.bmp")        ; load arkanix labs logo
  Global sp_ARKPROD = LoadSprite(#PB_Any, "gfx/hidden/alprod.bmp")        ; load arkanix labs logo
  Global sp_PUMPRAY = LoadSprite(#PB_Any, "gfx/hidden/pumpkinray.bmp")    ; load ray's pumpkin
  Global sp_R64LOGO = LoadSprite(#PB_Any, "gfx/hidden/retro64.bmp")       ; load r64 logo
  Global sp_SABSILL = LoadSprite(#PB_Any, "gfx/hidden/sill.bmp")          ; load silhouette
  
  a_C64WITCHANIMR(0) = LoadSprite(#PB_Any, "gfx/hidden/sprites/witchr1.bmp")   ; load c64 witch right animation frames
  a_C64WITCHANIMR(1) = LoadSprite(#PB_Any, "gfx/hidden/sprites/witchr2.bmp")       
  a_C64WITCHANIMR(2) = LoadSprite(#PB_Any, "gfx/hidden/sprites/witchr3.bmp")       
  a_C64WITCHANIMR(3) = LoadSprite(#PB_Any, "gfx/hidden/sprites/witchr4.bmp")
  a_C64WITCHANIML(0) = LoadSprite(#PB_Any, "gfx/hidden/sprites/witchl1.bmp")   ; load c64 witch left animation frames
  a_C64WITCHANIML(1) = LoadSprite(#PB_Any, "gfx/hidden/sprites/witchl2.bmp")       
  a_C64WITCHANIML(2) = LoadSprite(#PB_Any, "gfx/hidden/sprites/witchl3.bmp")       
  a_C64WITCHANIML(3) = LoadSprite(#PB_Any, "gfx/hidden/sprites/witchl4.bmp")
  
  a_C64GHOSTANIMR(0) = LoadSprite(#PB_Any, "gfx/hidden/sprites/ghostr1.bmp")   ; load c64 ghost right animation frames
  a_C64GHOSTANIMR(1) = LoadSprite(#PB_Any, "gfx/hidden/sprites/ghostr2.bmp")
  a_C64GHOSTANIMR(2) = LoadSprite(#PB_Any, "gfx/hidden/sprites/ghostr3.bmp")
  a_C64GHOSTANIMR(3) = LoadSprite(#PB_Any, "gfx/hidden/sprites/ghostr4.bmp")
  a_C64GHOSTANIMR(4) = LoadSprite(#PB_Any, "gfx/hidden/sprites/ghostr5.bmp")
  a_C64GHOSTANIMR(5) = LoadSprite(#PB_Any, "gfx/hidden/sprites/ghostr6.bmp")
  a_C64GHOSTANIML(0) = LoadSprite(#PB_Any, "gfx/hidden/sprites/ghostl1.bmp")   ; load c64 ghost left animation frames
  a_C64GHOSTANIML(1) = LoadSprite(#PB_Any, "gfx/hidden/sprites/ghostl2.bmp")
  a_C64GHOSTANIML(2) = LoadSprite(#PB_Any, "gfx/hidden/sprites/ghostl3.bmp")
  a_C64GHOSTANIML(3) = LoadSprite(#PB_Any, "gfx/hidden/sprites/ghostl4.bmp")
  a_C64GHOSTANIML(4) = LoadSprite(#PB_Any, "gfx/hidden/sprites/ghostl5.bmp")
  a_C64GHOSTANIML(5) = LoadSprite(#PB_Any, "gfx/hidden/sprites/ghostl6.bmp")
  
  a_C64OWLANIMR(0) = LoadSprite(#PB_Any, "gfx/hidden/sprites/owlr1.bmp")        ; load c64 owl right animation frames
  a_C64OWLANIMR(1) = LoadSprite(#PB_Any, "gfx/hidden/sprites/owlr2.bmp")
  a_C64OWLANIMR(2) = LoadSprite(#PB_Any, "gfx/hidden/sprites/owlr3.bmp")
  a_C64OWLANIMR(3) = LoadSprite(#PB_Any, "gfx/hidden/sprites/owlr4.bmp")
  a_C64OWLANIMR(4) = LoadSprite(#PB_Any, "gfx/hidden/sprites/owlr5.bmp")
  a_C64OWLANIML(0) = LoadSprite(#PB_Any, "gfx/hidden/sprites/owll1.bmp")        ; load c64 owl left animation frames
  a_C64OWLANIML(1) = LoadSprite(#PB_Any, "gfx/hidden/sprites/owll2.bmp")
  a_C64OWLANIML(2) = LoadSprite(#PB_Any, "gfx/hidden/sprites/owll3.bmp")
  a_C64OWLANIML(3) = LoadSprite(#PB_Any, "gfx/hidden/sprites/owll4.bmp")
  a_C64OWLANIML(4) = LoadSprite(#PB_Any, "gfx/hidden/sprites/owll5.bmp")
  
  a_C64ZOMBIEANIMR(0) = LoadSprite(#PB_Any, "gfx/hidden/sprites/zombier1.bmp")  ; load c64 zombie right animation frames
  a_C64ZOMBIEANIMR(1) = LoadSprite(#PB_Any, "gfx/hidden/sprites/zombier2.bmp")
  a_C64ZOMBIEANIMR(2) = LoadSprite(#PB_Any, "gfx/hidden/sprites/zombier3.bmp")
  a_C64ZOMBIEANIMR(3) = LoadSprite(#PB_Any, "gfx/hidden/sprites/zombier4.bmp")
  a_C64ZOMBIEANIML(0) = LoadSprite(#PB_Any, "gfx/hidden/sprites/zombiel1.bmp")  ; load c64 zombie left animation frames
  a_C64ZOMBIEANIML(1) = LoadSprite(#PB_Any, "gfx/hidden/sprites/zombiel2.bmp")
  a_C64ZOMBIEANIML(2) = LoadSprite(#PB_Any, "gfx/hidden/sprites/zombiel3.bmp")
  a_C64ZOMBIEANIML(3) = LoadSprite(#PB_Any, "gfx/hidden/sprites/zombiel4.bmp")    
  
 
  Global m_CHOON2 = LoadSound(#PB_Any, "sfx/mourn.ogg")                        ; and the hidden part music
  
EndProcedure  

Procedure DrawScanlines()
  
  ; this procedure draws the scanline sprite over the top of the images being
  ; displayed, to simulate scanlines, if they're switched on by the person viewing  
  
  Shared scanlines
  
  If scanlines = 1
    TransparentSpriteColor(sp_SCANLINES, RGB(255,255,255))
    DisplayTransparentSprite(sp_SCANLINES,0,0, 150)         ; 150/255 transparency so not too visible
  EndIf   
  
EndProcedure

Procedure DrawScanlinesSmall()
  
  ; this procedure draws the small scanline sprite over the top of the images being
  ; displayed in the hidden c64 part, to simulate scanlines if they're switched on
  ; by the person viewing the demo  
  
  Shared scanlines
  
  If scanlines = 1
    TransparentSpriteColor(sp_SCANLINESSMALL, RGB(255,255,255))
    DisplayTransparentSprite(sp_SCANLINESSMALL,200,121,50)       ; only 75/255 tranparency so not to 
  EndIf                                                         ; cover the c64 bitmaps completely
  
EndProcedure

Procedure FadeToBlack()
  
  Shared flash, scanlines
  
  ; a clear screen command is here just in case there are any left overs from
  ; creating the font sprites from earlier when loading the assets.  the clear 
  ; screen wipes the window completely white.
  
  ClearScreen(RGB(255,255,255))                   ; clears screen to white
  FlipBuffers()                                   ; shows the white screen
  Delay(100)
  
  fadelog=250
  
  For fade = 250 To 0 Step -25                   ; a little loop that changes the white screen from just above
                                                  ; through shades of grey until black is reached.
    ClearScreen(RGB(fadelog,fadelog,fadelog))     ; all this looks like a screen flash when running!
    
    If fadelog = 250
      
      If flash = 0
        DisplaySprite(sp_PUMPKIN, 0, 0)            ; when the screen is at almost maximum white, one of the
      EndIf                                       ; 'scarey' silhouette images is displayed briefly. 
                                                   ; each time this routine is run, the 'flash' variable is
      If flash = 1                                 ; increased by 1 so a different image is displayed
        DisplaySprite(sp_MASK, 0, 0)               ; every flash
      EndIf
      
      If flash = 2
        DisplaySprite(sp_GHOSTY, 0, 0)
      EndIf
      
      If flash = 3
        DisplaySprite(sp_SKULL, 0, 0)
      EndIf      
      
      If flash = 4
        DisplaySprite(sp_CLEAVER, 0, 0)
      EndIf      
      
    EndIf
    
    DrawScanlines()
    
    FlipBuffers()                                 ; swap to the buffer screen to make visible what we've drawn!
    
    If fadelog = 250
      Delay(150)
    EndIf    
    
    fadelog - 25
    
  Next fade  
  
EndProcedure 

Procedure CreateStarfield()
  
  ; rather than load a previosuly created starfield image for use behind the moon in
  ; the demo, the starfield is created here while the screen is hidden at the start.
  ; the stars are randomly generated, all 3000 of them, so the starfield is different
  ; every time the demo is run
  
  Global sp_STARFIELD = CreateSprite(#PB_Any,#XRES,#YRES)
  StartDrawing(SpriteOutput(sp_STARFIELD))
  For n=0 To 2999
    Plot(Random(#XRES-1),Random(#YRES-1),RGB(100,100,100))
  Next
  StopDrawing()
    
EndProcedure

Procedure DrawStarfield()
  
  ; this procedure draws the starfield that was created in 'CreateStarfield'
  ; it is scrolled from right to left by 1 pixel every 1 1/2 seconds.
  
  Shared bgx1
  
  For s1 = 0 To 1
    For s2 = 0 To 1
      DisplayTransparentSprite(sp_STARFIELD,#XRES*s1-bgx1,#YRES*s2)
    Next
  Next
  
  If ElapsedMilliseconds() - starfieldscrolltime > 90  ; 90 = 90th of a second
    starfieldscrolltime  = ElapsedMilliseconds()
    bgx1 + 1 : If bgx1 > #XRES-1 : bgx1=0 : EndIf     ; increase x pos by 1 pixel
  EndIf
  
EndProcedure    

Procedure DrawGraveYard()
  
  ; this displays the graveyard silhouette image!
  ; to make this work as a silhouette, white is set as
  ; transparent colour.  if you open the graveyard image
  ; in the graphics folder in any image editor, you will see
  ; that the 'sky' is white.  when displayed here, you do
  ; not see the white as it's set as transparent, so you see
  ; the moon behind it instead.  make sense?
  
  TransparentSpriteColor(sp_GRAVEYARD, RGB(255,255,255)) ; white is transparent
  DisplayTransparentSprite(sp_GRAVEYARD, 0, 0)
  
EndProcedure

Procedure AnimateSpider()
  
  Shared spider      ; the state of the spider sprite (1=animated, 0=static)
  Static spiderframe ; variable controlling the animation frame number we are using
  
  If spider = 0       ; spider is static
    spiderframe = 0   ; use only 1 frame of the animation
    TransparentSpriteColor(a_SPIDERANIM(spiderframe), RGB(255,255,255)) ; any white is transparent
    DisplayTransparentSprite(a_SPIDERANIM(spiderframe), 39, 258) ; display 1 frame
  EndIf
  
  If spider = 1       ; spider is animating
    TransparentSpriteColor(a_SPIDERANIM(spiderframe), RGB(255,255,255))
    DisplayTransparentSprite(a_SPIDERANIM(spiderframe), 39, 258)
    If ElapsedMilliseconds() - spideranimtime > 180	  ; 180 = every 180th of a second      
      spideranimtime  = ElapsedMilliseconds()		      ; reset timer to count again             
      spiderframe + 1                                 ; use next animation frame in array   
      If spiderframe = 9 : spiderframe = 0 :EndIf    ; if the end frame is reached, go
    EndIf                                            ; back to starting frame
  EndIf

EndProcedure  

Procedure AnimateHand()
  
  ; this procedure uses the same technique as the spider.  see the procedure
  ; 'AnimateSpider' for an explanation of what each part below is doing
  
  Shared hand
  Static handframe
  
  If hand = 0
    handframe = 0
    TransparentSpriteColor(a_HANDANIM(handframe), RGB(255,255,255))
    DisplayTransparentSprite(a_HANDANIM(handframe), 254, 404)
  EndIf
  
  If hand = 1
    TransparentSpriteColor(a_HANDANIM(handframe), RGB(255,255,255))
    DisplayTransparentSprite(a_HANDANIM(handframe), 254, 404)
    If ElapsedMilliseconds() - handanimtime > 300  ; every 300th of a second     
      handanimtime  = ElapsedMilliseconds()		             
      handframe + 1                                       
      If handframe = 8 : handframe = 0 :EndIf  
    EndIf
  EndIf
  
EndProcedure  
  
Procedure AnimateWindow()
  
  ; this procedure uses the same technique as the spider.  see the procedure
  ; 'AnimateSpider' for an explanation of what each part below is doing
  ; the only difference here is that the sprite is displayed 5 times in
  ; different positions because there are 5 windows
  
  
  Static windowframe ; variable controlling the animation frame number we are using
  
  If lights = 1 ; is the sprite on and animating?
  
  DisplayTransparentSprite(a_WINDOWANIM(windowframe), 691, 272)
  DisplayTransparentSprite(a_WINDOWANIM(windowframe), 607, 324)
  DisplayTransparentSprite(a_WINDOWANIM(windowframe), 740, 327)
  DisplayTransparentSprite(a_WINDOWANIM(windowframe), 639, 390)
  DisplayTransparentSprite(a_WINDOWANIM(windowframe), 708, 391)    
  
  If ElapsedMilliseconds() - windowanimtime > 240	 ; every 240th of a second     
    windowanimtime  = ElapsedMilliseconds()		             
    windowframe + 1                                       
    If windowframe = 8 : windowframe = 0 :EndIf  
  EndIf
  
  EndIf
  
EndProcedure  
  
Procedure AnimateBats()
  
  ; this procedure uses the same animation technique as the spider.  see the procedure
  ; 'AnimateSpider' for an explanation of what each part below is doing
  ; it's a little different because the sprite is displayed 3 times in slightly
  ; different x/y positions so it looks like more bats!
  ; the biggest difference is further below because the bats move around
  ; following a set path. see below for an explantion!  
  
  Shared bats
  Static batframe ; variable controlling the animation frame number we are using
  
  If bats = 1
    
    TransparentSpriteColor(a_BATANIM(batframe), RGB(255,255,255)) ; white is transparent
    DisplayTransparentSprite(a_BATANIM(batframe), batx, baty)
    DisplayTransparentSprite(a_BATANIM(batframe), batx - 15, baty + 15)
    DisplayTransparentSprite(a_BATANIM(batframe), batx + 30, baty + 25)
    
    If ElapsedMilliseconds() - batanimtime > 35	        
      batanimtime  = ElapsedMilliseconds()		             
      batframe + 1                                       
      If batframe = 4 : batframe = 0 :EndIf  
    EndIf
    
  ; here is the different part to the spider   
    
    If batpath = 1            ; the first path of the flying bats
      batx - 2 : baty - 2     ; diagonal up and left
      If batx = 550           ; have the bats reached x position 550?
        batpath = 2           ; if, so, bats follow path 2!
      EndIf
    EndIf
    
    If batpath = 2            ; bats path 2
      batx - 3 : baty + 2     ; diagonal down and left
      If batx = 400           ; until bats x position is 400
        batpath = 3           ; then do path 3
      EndIf
    EndIf
    
    If batpath = 3             ; bats path 3
      batx + 2 : baty + 1       ; diagonal down and right
      If baty = 350
        batpath = 4
      EndIf
    EndIf
    
    If batpath = 4              ; bats path 4
      batx + 3 : baty - 1       ; diagonal up and right
      If batx > 2000            ; if bats reach x position 2000, way off screen
        batpath = 1 : batx = 650 : baty = 325 ; reset to bats path 1 and xpos 650
      EndIf                                  ; and y pos 325
    EndIf
    
  EndIf
  
EndProcedure  

Procedure AnimateWitch()
  
  Shared witchx, witch     ; variable controlling the x position of the witch
  Static witchframe        ; variable controlling the animation frame number we are using
  
  If witch = 1
    
    If witchx = 500
      witchy = 96
      witchzoomx = 60
      witchzoomy = 55
    EndIf
    
    If witchx = 400
      witchy = 92
      witchzoomx = 58
      witchzoomy = 53    
    EndIf
    
    If witchx = 300
      witchy = 88
      witchzoomx = 50
      witchzoomy = 46     
    EndIf
    
    If witchx = 220
      witchy = 82
      witchzoomx = 46
      witchzoomy = 42     
    EndIf   
    
    If witchx > -100
      witchx - 2
      If witchx = -100
        witchx = 850 : witchy = 100
        witchzoomx = 68 : witchzoomy = 62 
      EndIf
    EndIf
    
    TransparentSpriteColor(a_WITCHANIM(witchframe), RGB(255,255,255))
    ZoomSprite(a_WITCHANIM(witchframe), witchzoomx, witchzoomy)
    DisplayTransparentSprite(a_WITCHANIM(witchframe), witchx, witchy)
    
    If ElapsedMilliseconds() - witchanimtime > 240      
      witchanimtime  = ElapsedMilliseconds()		             
      witchframe + 1                                       
      If witchframe = 4 : witchframe = 0 :EndIf  
    EndIf
    
    ; this next little check sees if the witch has flown a certain distance across the screen
    ; and if so plays a witch cackle sound. the sound doesn't play every time the witch crosses
    ; the screen - there is only a 1 in 5 chance. every time would be to much!
    
    If witchx = 650                                       ; has the witch reached x position 650?
      witchcackle = Random(4)                             ; generate a random number between 0 and 4
      If witchcackle = 0                                  ; if random number is 0
        PlaySound(s_CACKLE, #PB_Sound_MultiChannel, 25)   ; play a witch cackle sound
      EndIf
    EndIf
    
  EndIf 
  
EndProcedure  

Procedure AnimateOwl()
  
  ; this owl animation procedure is a little different to the others because each
  ; individual frame isn't stored in an array.  each frame is 'cut' or 'clipped'
  ; from the main image that contains all the frames.
  
  Shared owlx, owl ; variable controlling the x position of the owl and is the owl on or off
  
  Static owlframe ; variable controlling the animation frame number we are using
  
  If owl = 1 ; has the person switched the owl on?
  
  ; look at the owl bitmap image in the graphics folder.  it contains all the frames to animate the owl.
  ; basically, to animate the owl, the relevant part of the image is displayed when needed.
  
  ; first, clip (cut) the image based on the frame number multiplied by the width of the
  ; owl sprite, which in this case is 32 pixels, to leave just the part of the image we need.
  
  ClipSprite(sp_OWL, owlframe * 32, 0, 32, 89)
  
  ; now show the animation frame that is already clipped above
  TransparentSpriteColor(sp_OWL, RGB(255,255,255)) ; white is transparent
  ZoomSprite(sp_OWL, 16, 44) ; make the image smaller so the owl seems distant
  DisplayTransparentSprite(sp_OWL, owlx, 350) ; show the owl!
  
  ; now we check the timer to see if it's time to show the next frame in the animation.
  ; if so, the frame number is increased and the timer restarted.
  
  If ElapsedMilliseconds() - owlanimtime > 60 / 2       ; check if half a second has past
    owlanimtime = ElapsedMilliseconds()                 ; if so, restart the timer
    owlframe + 1                                        ; increase the frame number
    If owlframe=16 : owlframe=0 : EndIf                ; check to see if the last frame has been
  EndIf                                                ; reached, if so, reset to start frame!
  
  owlx - 4 : If owlx < -1000 : owlx = 2000 :  EndIf     ; change the owl sprite x position on screen
  
 EndIf 
  
EndProcedure

Procedure AnimateZombie()
  
  Shared zombiex, zombie     ; variable controlling the x position of the zombie and whether it's on or off
  Static zombieframe ; variable containing the animation frame number we are using
  
  If zombie = 1 ; has the zombie been switched on
    
    zombiex + 2         ; move the zombie x postion 2 pixels right
    
    If zombiex > 850    ; has the zombie reached greater than x pos 850?
      zombiex = -400    ; if yes, reset zombie x position to -400 off the left of the screen
    EndIf
    
  TransparentSpriteColor(a_ZOMBIEANIM(zombieframe), RGB(255,255,255)) ; white is transparent
  ZoomSprite(a_ZOMBIEANIM(zombieframe), 377, 400) ; make the zombie image a little smaller
  DisplayTransparentSprite(a_ZOMBIEANIM(zombieframe), zombiex, 145) ; draw the zombie! argh!
  
  If ElapsedMilliseconds() - zombieanimtime > 120   ; every 120th of a second
    zombieanimtime  = ElapsedMilliseconds()		             
    zombieframe + 1                                       
    If zombieframe = 7 : zombieframe = 0 :EndIf  
  EndIf
  
  ; this next little check sees if the zombie has walked a certain distance across the screen
  ; and if so plays a zombie groan sound.  the sound doesn't play every time the zombie crosses
  ; as there is only a 1 in 3 chance. every time would be to much!
  
  If zombiex = 50                                       ; has the zombie reached x position 50?
    zombiegroan = Random(2)                             ; generate a random number between 0 and 3
    If zombiegroan = 0                                  ; if random number is 0
      PlaySound(s_GROAN, #PB_Sound_MultiChannel, 50)   ; play a zombie groan sound
    EndIf
  EndIf
  
 EndIf 
  
EndProcedure  

Procedure AnimateGhost()
  
  Shared ghostx, ghosty, ghost, ghostdisplay ; these must be 'remembered' between each procedure call!
  Static ghostframe ; the frame number of the ghost being displayed
  
  If ghost = 1 ; has the person viewing the demo switched the ghost on?
    
    ; this timer checks to see if 12 seconds has past and if so, it resets the timer to
    ; count another 8, switches on the ghost (ghostdisplay=1), then generates a
    ; random number between 50 and 750 which becomes the shooting ghost x position at
    ; the bottomish of the screen.
    
    If ElapsedMilliseconds() - ghosttimer > 12000 ; 12 seconds has passed yet?
      ghosttimer  = ElapsedMilliseconds()         ; reset the timer and start counting again
      ghostdisplay = 1                            ; switch the ghost on
      ghostx = Random(#XRES - 275, #XRES - 700)   ; generate a random x position for the ghost
    EndIf
    
    ; if the ghost is switched on in the timer above (every 8 seconds) then the
    ; ghost is displayed.  it starts off screen at 'y' position +650 and then each
    ; time this procedure is called the ghost sprite position is adjusted by
    ; subracting 5 pixels from the 'y' position (upwards)
    
    If ghostdisplay = 1 ; should the ghost be displayed?  controlled by timer not user!!!!
      TransparentSpriteColor(a_GHOSTANIM(ghostframe), RGB(255,255,255)) ; white is transparent
      ZoomSprite(a_GHOSTANIM(ghostframe), 37, 44) ; make the ghost a little smaller
      DisplayTransparentSprite(a_GHOSTANIM(ghostframe), ghostx, ghosty) ; draw the ghost!
      ghosty - 5 ; move the ghost 5 pixels upwards
      If ElapsedMilliseconds() - ghostanimtime > 60   ; every 60th second     
        ghostanimtime  = ElapsedMilliseconds()		             
        ghostframe + 1                                       
        If ghostframe = 6 : ghostframe = 0 :EndIf  
      EndIf
      
    ; once the 'y' position reaches -100 at the top of the screen, the ghost sprite 
    ; is switched off temporarily and the 'y' position reset to +650, at the bottom of the screen.     
      
      If ghosty < -100
        ghostdisplay = 0
        ghosty = 650
      EndIf
      
      ; this next little check sees if the ghost has flown a certain distance up the screen
      ; and if so plays a ghost howl sound. the sound doesn't play every time the ghost travels
      ; up the screen - there is only a 1 in 2 chance. every time would be to much!
      
      If ghosty = 600                                       ; has the ghost reached y position 600?
        ghosthowl = Random(1)                               ; generate a random number between 0 and 1
        If ghosthowl = 0                                    ; if random number is 0
          PlaySound(s_GHOST, #PB_Sound_MultiChannel, 25)    ; play the ghost howl sound
        EndIf
      EndIf      
      
      
    EndIf
    
  EndIf
  
EndProcedure 

Procedure AnimateC64Sprites()
  
  ; this entire procedure controls all the c64 sprites in the hidden demo part
  ; they do not need their own procedures because only one of them is displayed at a time
  ; they are never all together on the screen
  ; see the 'AnimateSpider' procedure for an explanation or what each section is doing
  ; any differences to the spider are explained below...
  
  Shared c64spritex             ; variable controlling the x position of the c64 sprites
  Shared C64sprite              ; which sprite to use?  witch, owl, zombie, ghost?
  Shared c64spritedir           ; which direction? 0 is move right, 1 is move left
  Static c64spriteframe         ; variable controlling the animation frame number we are using
  
  ; this next section controls 
  
  If c64spritedir = 0           ; if the c64 sprite is moving left to right on the screen
    If c64spritex < 650         ; is the sprite x position less than 650?
      c64spritex + 1            ; if so, move the sprite right by 1 pixel
      If c64spritex = 650       ; has the sprite reached x posisiton 650?
        c64spritedir = 1        ; if so, the sprite should now move right to left...
        c64spriteframe = 0
      EndIf
    EndIf    
  EndIf

  If c64spritedir = 1           ; if the c64 sprite is moving right to left on the screen
    If c64spritex > 150         ; is the sprite x position bigger than 150?
      c64spritex - 1            ; if so, move the sprite left by 1 pixel
      If c64spritex = 150       ; has the sprite reached x posisiton 150?
        c64spritedir = 0        ; if so, the sprite should now move left to right again!
        c64sprite + 1           ; add 1 to the sprite being displayed (0=witch, 1=ghost, 2=owl, 3=zombie)
        If c64sprite = 4        ; if we are up to '4' there are no more sprites so go back to '0'
          c64sprite = 0         ; which is the witch again
          c64spriteframe = 0
        EndIf
      EndIf
    EndIf    
  EndIf
  
  
  If c64sprite = 0 ; this loop draws the c64 witch flying right then left
    
    If c64spritedir = 0    
      TransparentSpriteColor(a_C64WITCHANIMR(c64spriteframe), RGB(255,255,255))
      ZoomSprite((a_C64WITCHANIMR(c64spriteframe)), 34, 32)
      DisplayTransparentSprite(a_C64WITCHANIMR(c64spriteframe), c64spritex, 178)
      If ElapsedMilliseconds() - C64spriteanimtime > 120      
        C64spriteanimtime  = ElapsedMilliseconds()		             
        c64spriteframe + 1                                      
        If c64spriteframe = 4 : c64spriteframe = 0 :EndIf  
      EndIf
    EndIf
    
    If c64spritedir = 1   
      TransparentSpriteColor(a_C64WITCHANIML(c64spriteframe), RGB(255,255,255))
      ZoomSprite((a_C64WITCHANIML(c64spriteframe)), 34, 32)
      DisplayTransparentSprite(a_C64WITCHANIML(c64spriteframe), c64spritex, 178)
      If ElapsedMilliseconds() - C64spriteanimtime > 120      
        C64spriteanimtime  = ElapsedMilliseconds()		             
        c64spriteframe + 1                                      
        If c64spriteframe = 4 : c64spriteframe = 0 :EndIf  
      EndIf
    EndIf
  
  EndIf
  
  If c64sprite = 1 ; this loop draws the c64 ghost flying right then left
    
    If c64spritedir = 0    
      TransparentSpriteColor(a_C64GHOSTANIMR(c64spriteframe), RGB(255,255,255))
      ZoomSprite((a_C64GHOSTANIMR(c64spriteframe)), 34, 32)
      DisplayTransparentSprite(a_C64GHOSTANIMR(c64spriteframe), c64spritex, 228)
      If ElapsedMilliseconds() - C64spriteanimtime > 120      
        C64spriteanimtime  = ElapsedMilliseconds()		             
        c64spriteframe + 1                                      
        If c64spriteframe = 6 : c64spriteframe = 0 :EndIf  
      EndIf
    EndIf
    
    If c64spritedir = 1   
      TransparentSpriteColor(a_C64GHOSTANIML(c64spriteframe), RGB(255,255,255))
      ZoomSprite((a_C64GHOSTANIML(c64spriteframe)), 34, 32)
      DisplayTransparentSprite(a_C64GHOSTANIML(c64spriteframe), c64spritex, 228)
      If ElapsedMilliseconds() - C64spriteanimtime > 120      
        C64spriteanimtime  = ElapsedMilliseconds()		             
        c64spriteframe + 1                                      
        If c64spriteframe = 6 : c64spriteframe = 0 :EndIf  
      EndIf
    EndIf
  
  EndIf  
  
  If c64sprite = 2 ; this loop draws the c64 owl flying right then left
    
    If c64spritedir = 0    
      TransparentSpriteColor(a_C64OWLANIMR(c64spriteframe), RGB(255,255,255))
      ZoomSprite((a_C64OWLANIMR(c64spriteframe)), 34, 32)
      DisplayTransparentSprite(a_C64OWLANIMR(c64spriteframe), c64spritex, 210)
      If ElapsedMilliseconds() - C64spriteanimtime > 120      
        C64spriteanimtime  = ElapsedMilliseconds()		             
        c64spriteframe + 1                                      
        If c64spriteframe = 5 : c64spriteframe = 0 :EndIf  
      EndIf
    EndIf
    
    If c64spritedir = 1   
      TransparentSpriteColor(a_C64OWLANIML(c64spriteframe), RGB(255,255,255))
      ZoomSprite((a_C64OWLANIML(c64spriteframe)), 34, 32)
      DisplayTransparentSprite(a_C64OWLANIML(c64spriteframe), c64spritex, 210)
      If ElapsedMilliseconds() - C64spriteanimtime > 120      
        C64spriteanimtime  = ElapsedMilliseconds()		             
        c64spriteframe + 1                                      
        If c64spriteframe = 5 : c64spriteframe = 0 :EndIf  
      EndIf
    EndIf
  
  EndIf    
  
  If c64sprite = 3 ; this loop draws the c64 zombie walking right then left
    
    If c64spritedir = 0    
      TransparentSpriteColor(a_C64ZOMBIEANIMR(c64spriteframe), RGB(255,255,255))
      ZoomSprite((a_C64ZOMBIEANIMR(c64spriteframe)), 34, 32)
      DisplayTransparentSprite(a_C64ZOMBIEANIMR(c64spriteframe), c64spritex, 235)
      If ElapsedMilliseconds() - C64spriteanimtime > 100     
        C64spriteanimtime  = ElapsedMilliseconds()		             
        c64spriteframe + 1                                      
        If c64spriteframe = 4 : c64spriteframe = 0 :EndIf  
      EndIf
    EndIf
    
    If c64spritedir = 1   
      TransparentSpriteColor(a_C64ZOMBIEANIML(c64spriteframe), RGB(255,255,255))
      ZoomSprite((a_C64ZOMBIEANIML(c64spriteframe)), 34, 32)
      DisplayTransparentSprite(a_C64ZOMBIEANIML(c64spriteframe), c64spritex, 235)
      If ElapsedMilliseconds() - C64spriteanimtime > 100     
        C64spriteanimtime  = ElapsedMilliseconds()		             
        c64spriteframe + 1                                      
        If c64spriteframe = 4 : c64spriteframe = 0 :EndIf  
      EndIf
    EndIf
  
  EndIf   
  
EndProcedure  

Procedure DrawClouds()
  
  Shared cloud1x, cloud2x ; remember the clouds x position between routine calls!
  
  ; the clouds sprites are exactly the same width as the window
  ; each time this routine is called, cloud 1 moves right and
  ; cloud2 moves left.  once each cloud has travelled it's full
  ; width, it resets.  basically, each cloud continously wraps on
  ; the screen.
  ;
  ; the clouds movement are timer controlled.  cloud1 moves right
  ; every 60th of a second, while cloud2 moves left every 120th
  ; of a second, a little slower than cloud1.
  
  
  TransparentSpriteColor(sp_CLOUD1, RGB(255,255,255))  
  DisplayTransparentSprite(sp_CLOUD1, cloud1x, 5)
  
  TransparentSpriteColor(sp_CLOUD2, RGB(255,255,255))  
  DisplayTransparentSprite(sp_CLOUD2, cloud2x, 105)  
  
  If ElapsedMilliseconds() - cloud1scrolltime > 60  
    cloud1scrolltime  = ElapsedMilliseconds()
    cloud1x + 1 : If cloud1x > 801 : cloud1x = -674 :  EndIf
  EndIf
  
  If ElapsedMilliseconds() - cloud2scrolltime > 120 
    cloud2scrolltime  = ElapsedMilliseconds()
    cloud2x - 1 : If cloud2x < -674 : cloud2x = 801 :  EndIf  
  EndIf
  
EndProcedure  

Procedure DrawSmallBloodDrips()
  
  Shared blooddrip1y, blooddrip2y, blooddrip3y ; these must be 'remembered' between each procedure call!
  Shared blooddrip1x, blooddrip2x, blooddrip3x
  Shared blooddripsmall
  
  ; this timer checks to see if 5 seconds has past and if so, it resets the timer to
  ; count another 5, switches on the small blood drips (blooddripsmall=1), then generates a
  ; random number between 50 and 750 which becomes the blood drips x position at
  ; the top of the screen.
  
  If ElapsedMilliseconds() - blooddriptimer > 1200
    blooddriptimer  = ElapsedMilliseconds()
    blooddripsmall = 1
    blooddrip1x = Random(#XRES - 50, #XRES - 750)
    blooddrip2x = Random(#XRES - 50, #XRES - 750)
    blooddrip3x = Random(#XRES - 50, #XRES - 750)
  EndIf
  
  ; if the small blood drips are switched on in the timer above (every 5 seconds) then the
  ; small blood drips are displayed.  it starts off screen at 'y' position -50 and then each
  ; time this procedure is called the blood drip sprites positions are adjusted by
  ; adding pixels to the 'y' position (downwards).
  ; once the 'y' position of blooddrip 1 reaches over 900, the blood drips are 
  ; switched off and the 'y' position reset to -50, off the top of the screen.
  
  If blooddripsmall = 1
    
    DisplayTransparentSprite(sp_BLOODSMALL, blooddrip1x, blooddrip1y)
    blooddrip1y + 40
    
    DisplayTransparentSprite(sp_BLOODSMALL, blooddrip2x, blooddrip2y)
    blooddrip2y + 60
    
    DisplayTransparentSprite(sp_BLOODSMALL, blooddrip3x, blooddrip3y)
    blooddrip3y + 20    
    
   If blooddrip3y > 850
      blooddripsmall = 0
      blooddrip1y = - 50
      blooddrip2y = - 50
      blooddrip3y = - 50
    EndIf
    
  EndIf
    
EndProcedure 

Procedure DrawMoon()
  
 ; moonrise = 20
  
  DisplayTransparentSprite(sp_MOON, 0, moonrisey)
  
  If ElapsedMilliseconds() - moonscrolltime > 60 
    moonscrolltime  = ElapsedMilliseconds()
    moonrisey - 1 : If moonrisey < 10 : moonrisey = 10 :  EndIf
  EndIf
  
EndProcedure  

Procedure DrawShootStar()
  
  Shared shootstarx, shootstary ; these must be 'remembered' between each procedure call!
  
  ; this timer checks to see if 10 seconds has past and if so, it resets the timer to
  ; count another 10, switches on the shooting star (shootstar=1), then generates a
  ; random number between 50 and 750 which becomes the shooting star x position at
  ; the top of the screen.
  
  If ElapsedMilliseconds() - shootstartimer > 10000
    shootstartimer  = ElapsedMilliseconds()
    shootstar = 1
    shootstarx = Random(#XRES - 50, #XRES - 750)
  EndIf
  
  ; if the shooting star is switched on in the timer above (every 10 seconds) then the
  ; shooting star is displayed.  it starts off screen at 'y' position -100 and then each
  ; time this procedure is called the shooting star sprite position is adjusted by
  ; adding 30 pixels to the 'y' position (downwards) and subtracting 10 pixels off
  ; the x position (move left).  once the 'y' position reaches over 800, the shooting
  ; star is switched off and the 'y' position reset to -100, off the top of the screen.
  
  If shootstar = 1
    DisplayTransparentSprite(sp_SHOOTSTAR, shootstarx, shootstary, 150)
    shootstary + 30 : shootstarx - 10
    If shootstary > 800
      shootstar = 0
      shootstary = - 100
    EndIf
  EndIf
    
EndProcedure 

Procedure DrawScroller1()
  
  ; the scrolling messages are controlled and updated here.
  ; try adjusting the various variables to see the effect on the scrollers!
  
  
  Shared cco1, cc1, t1$, tptr1, sco1, m1 ; remember these variable between each procedure call!
  
  ; bottom sine wave scroller, with larger sprites
  
  cco1=1 ; where scroll disappears from screen
  For cc1=0 To 25 ; where scroll appears on screen
    letter=(Asc(Mid(t1$,tptr1+cc1,1))-31) ; detect the next letter in the scroll message
    ZoomSprite(a_gfxfont(letter), 116, 40); assign the letter a sprite from the array and zoom to correct size
    DisplayTransparentSprite(a_gfxfont(letter),(sco1+cco1),500+60*Sin((cc1+cco1+sco1+m1)/200),15) ; display sprite!
    DisplayTransparentSprite(a_gfxfont(letter),(sco1+cco1),500-60*Sin((cc1+cco1+sco1+m1)/200),15) ; display sprite!    
    DisplayTransparentSprite(a_gfxfont(letter),(sco1+cco1),500+20*Sin((cc1+cco1+sco1+m1)/150),25) ; display sprite!
    DisplayTransparentSprite(a_gfxfont(letter),(sco1+cco1),500-20*Sin((cc1+cco1+sco1+m1)/150),25) ; display sprite!
    DisplayTransparentSprite(a_gfxfont(letter),(sco1+cco1),500+40*Sin((cc1+cco1+sco1+m1)/150),40) ; display sprite!
    DisplayTransparentSprite(a_gfxfont(letter),(sco1+cco1),500-40*Sin((cc1+cco1+sco1+m1)/150),40) ; display sprite!     
    DisplayTransparentSprite(a_gfxfont(letter),(sco1+cco1),500+15*Sin((cc1+cco1+sco1+m1)/100)) ; display sprite!
    cco1=cco1+70
  Next
  m1=m1-5
  sco1=sco1-7 ; scroll speed
  If sco1<-70
    tptr1=tptr1+1 ; scroll smoothness
    sco1=sco1+70
  EndIf
  If tptr1>(Len(t1$)-30 )
    tptr1=1
  EndIf
  

EndProcedure  

Procedure DrawScroller2()
  
  ; the scrolling messages are controlled and updated here.
  ; try adjusting the various variables to see the effect on the scrollers!
  
  
  Shared cco2, cc2, t2$, tptr2, sco2, m2
  

  
  ; top straight line scroller, with smaller sprites.
  
  cco2=-500 ; where scroll disappears from screen
  For cc2=0 To 25 ; where scroll appears on screen
    letter=(Asc(Mid(t2$,tptr2+cc2,1))-31)
    ZoomSprite(a_gfxfont(letter), 45, 32)
    ;DisplayTransparentSprite(a_gfxfont(letter), 625, (sco2+cco2))
    DisplayTransparentSprite(a_gfxfont(letter),625+15*Sin((cc2+cco2+sco2+m2)/100),(sco2+cco2)+5, 50) 
    DisplayTransparentSprite(a_gfxfont(letter),620+15*Sin((cc2+cco2+sco2+m2)/100),(sco2+cco2), 125)
    cco2=cco2+35
  Next
  m2=m2-3
  sco2=sco2-3 ; scroll speed
  If sco2<-35
    tptr2=tptr2+1 ; scroll smoothness
    sco2=sco2+35
  EndIf
  If tptr2>(Len(t2$)-31 )
    tptr2=1
  EndIf  

EndProcedure  

Procedure DrawScroller3()
  
  ; the scrolling messages are controlled and updated here.
  ; try adjusting the various variables to see the effect on the scrollers!
  
  
  Shared cco3, cc3, t3$, tptr3, sco3, m3, scroll3y, scrolldir
  
  ; bottom sine wave scroller, with larger sprites
  
  cco3=150 ; where scroll disappears from screen
    For cc3=0 To 30 ; where scroll appears on screen
      letter=(Asc(Mid(t3$,tptr3+cc3,1))-31)
      ZoomSprite(a_gfxfont(letter), 16, 13)
      DisplayTransparentSprite(a_gfxfont(letter), (sco3+cco3), scroll3y) 
      cco3=cco3+17
    Next
  
  m3=m3-3
  
  sco3=sco3-3 ; scroll speed
  
  If sco3<-17
    tptr3=tptr3+1 ; scroll smoothness
    sco3=sco3+17
  EndIf
  If tptr3>(Len(t3$)-31)
    tptr3=1
  EndIf
  
  If scrolldir = 1
    scroll3y + 1
    If scroll3y => 345
      scrolldir = 0
    EndIf
  EndIf
  
  If scrolldir = 0
    scroll3y - 1
    If scroll3y =< 315
      scrolldir = 1
    EndIf
  EndIf 
  
EndProcedure  

Procedure DrawFades()
  
  ; very simple stuff here. draw two 'fades' on each side of the screen where the main
  ; scroller enters and leaves the screen.  the fades are simple 'hatched' images with
  ; the hatching increasing towards the edge of the screen to suggest a simple fade.
  
  TransparentSpriteColor(sp_LEFTFADE, RGB(255,255,255))
  DisplayTransparentSprite(sp_LEFTFADE, 0, 445)
  TransparentSpriteColor(sp_RIGHTFADE, RGB(255,255,255))
  DisplayTransparentSprite(sp_RIGHTFADE, 500, 445)  
  
EndProcedure  

Procedure KeyChecker()
  
  ; this procedure checks which keys, if any, the demo viewer has pressed
  
  ; first, remember the values of these variables between routine calls and share!
  Shared spider, witch, bats, owl, zombie, ghost, hand, scanlines, demopart, subpart, flash
  
  
  ; this key press (right shift and enter simultaneously) makes the demo enter the
  ; hidden part.  when these keys are pressed the main music is stopped and cleared out,
  ; the screen is cleared with a flash and a timer set ready to count time between the
  ; display of each c64 bitmap from sabbat
  
  If KeyboardPushed(#PB_Key_RightShift) And KeyboardReleased(#PB_Key_Return)
    demopart = 4
    StopSound(m_CHOON)
    FreeSound(m_CHOON)
    flash + 1
    FadeToBlack()
    c64displaytime = ElapsedMilliseconds() 
  EndIf
  
  ; this simple key press (space) is for the hidden part of the demo
  ; and will display the c64 silhouette image
  
  If KeyboardPushed(#PB_Key_Space) And subpart = 7
    subpart = 8
  EndIf  
  
  ; this keypress (TAB) switches scanline emulation on and off
  
  If KeyboardReleased(#PB_Key_Tab)
    If scanlines = 0
      scanlines = 1
    Else
      scanlines = 0
    EndIf
  EndIf
  
  ; this keypress (L) switches on/off the main demo house lights
  
  If KeyboardReleased(#PB_Key_L)
    If lights = 0
      lights = 1
    Else
      lights = 0
    EndIf
  EndIf
  
  ; this keypress (H) switches on/off the main demo graveyard hand
  
  If KeyboardReleased(#PB_Key_H)
    If hand = 0
      hand = 1
    Else
      hand = 0
    EndIf
  EndIf  
  
  ; this keypress (S) switches on/off the main demo house spider animation 
  
  If KeyboardReleased(#PB_Key_S)
    If spider = 0
      spider = 1
    Else
      spider = 0
    EndIf
  EndIf
  
  ; this keypress (W) switches on/off the main demo witch
  
  If KeyboardReleased(#PB_Key_W)
    If witch = 0
      witch = 1
    Else
      witch = 0
    EndIf
  EndIf
  
  ; this keypress (B) switches on/off the main demo bats 
  
  If KeyboardReleased(#PB_Key_B)
    If bats = 0
      bats = 1
    Else
      bats = 0
    EndIf
  EndIf
  
  ; this keypress (O) switches on/off the main demo owl 
  
  If KeyboardReleased(#PB_Key_O)
    If owl = 0
      owl = 1
    Else
      owl = 0
    EndIf
  EndIf
  
  ; this keypress (Z) switches on/off the main demo zombie 
  
  If KeyboardReleased(#PB_Key_Z)
    If zombie = 0
      zombie = 1
    Else
      zombie = 0
    EndIf
  EndIf
  
  ; this keypress (G) switches on/off the main demo ghost
  
  If KeyboardReleased(#PB_Key_G)
    If ghost = 0
      ghost = 1
    Else
      ghost = 0
    EndIf
  EndIf
  
  ; this keypress (DEL) switches on/off all the sprites at the same time
  
  If KeyboardReleased(#PB_Key_Delete)
    If allsprites = 0
      allsprites = 1
      lights = 1 : spider = 1 : witch = 1 : bats = 1 : owl = 1 : zombie = 1 : ghost = 1 : hand = 1
    Else
      allsprites = 0
      lights = 0 : spider = 0 : witch = 0 : bats = 0 : owl = 0 : zombie = 0 : ghost = 0 : hand = 0
    EndIf
  EndIf 
  
EndProcedure  


;- BEGIN PROGRAM ----------------------------------------------------       
;
; here's where the everything actually starts!
;
; we call the various procedures above to set everything up and then
; start the actual demo playing in the main loop below...


OpenMainWindow()                         ; open the window on the user's desktop
DisplayLoader()                          ; display the 'loading...' message while everything loads
CreateStarfield()                        ; play god! create stars for the background ;)
LoadAssets()                             ; load all the image and music files
FadeToBlack()                            ; run the 'fadetoblack' procedure to flash the screen
PlaySound(m_CHOON, #PB_Sound_Loop, vol)  ; start playing the music


;- MAIN PROGRAM LOOP ------------------------------------------------  

; now call the various procedures above in a repeat loop, drawing the
; various parts of the demoscreen
;
; the loop 'Repeats' 'Until' the the 'escape' key is pressed
;
; the loop below is divided into various parts and subparts.  each 'demopart'
; displays on the screen the part we are on, whether it be part of the intro
; title cards, or the main demo screen. after each part is displayed, the
; demopart variable is increased by 1 so the next part will be displayed.
; some demoparts also have sub-parts


Repeat
  
  Delay(0)                        ; allow some time for the multitasking environment!
  
  WaitWindowEvent(0)              ; wait for an event in our window ('escape' keypress)
  
  ClearScreen(0)                  ; clear the screen to black to redraw/update everything
  
    
 ;{ DemoPart = 1 --> Display Cosine Title Card 
  
  If demopart = 1
    
    DisplayTransparentSprite(sp_COSPRES,0,0, transparency)
    
    If transparency < 255
      transparency + 10
      If transparency > 255
        transparency = 255
      EndIf
    EndIf
    
    DrawSmallBloodDrips()
    
    DisplayTransparentSprite(sp_BLOODDRIP,0,bloody)
    
    bloody + 3
    
    DrawScanlines()
    
    If bloody = 0
      bloody = -1800
      demopart=2
      blooddrip1y = - 50
      blooddrip2y = - 50
      blooddrip3y = - 50
      flash + 1
      transparency = 0
      FadeToBlack()
    EndIf
    
  EndIf
  
;}  
  
 ;{ DemoPart = 2 --> Display DBF Title Card 
  
  If demopart = 2
    
    DisplayTransparentSprite(sp_DBFCHALL,0,0,transparency)
    
    If transparency < 255
      transparency + 10
      If transparency > 255
        transparency = 255
      EndIf
    EndIf
   
    
    DrawSmallBloodDrips()
    
    DisplayTransparentSprite(sp_BLOODDRIP,0,bloody)
    
    bloody + 3
    
    DrawScanlines()
    
    If bloody = 0
      bloody = -1800
      demopart=3
      flash + 1
      FadeToBlack()
    EndIf    
    
  EndIf
  
;}  
  
 ;{ DemoPart = 3 --> Display Main Silhouette Demo Screen 
  
  If demopart = 3
    
    DrawStarfield()
    DrawShootStar()
    DrawMoon()
    DrawScroller2()
    DrawGraveYard()
    AnimateSpider()
    AnimateHand()
    AnimateWitch()
    AnimateWindow()
    AnimateGhost()
    AnimateBats()
    AnimateOwl()
    DrawClouds()
    AnimateZombie()
    DrawScroller1()
    DrawFades()
    DrawScanlines()
    
  EndIf
  
;}  
  
 ;{ DemoPart = 4 --> Display Hidden Part / Sabbat Remake 
  
  If demopart = 4
    
    If subpart = 1 ; display the c64 boot screen then loads c64 assets at the bottom

      DisplaySprite(sp_C64BOOT,224,146)
      
      If ElapsedMilliseconds() - c64displaytime > 4000 ; 4 second
        c64displaytime = ElapsedMilliseconds()  
        subpart = 2
        PlaySound(m_CHOON2, #PB_Sound_Loop, vol)  ; start playing the music
      EndIf
      
    EndIf  
    
    If subpart = 2 ; display the arkanix labs production title card
      
      DisplaySprite(sp_ARKPROD,224,146)
      
      If ElapsedMilliseconds() - c64displaytime > 4000 ; 4 seconds
        c64displaytime = ElapsedMilliseconds()  
        subpart = 3
      EndIf
      
    EndIf      
    
    If subpart = 3 ; display R64 logo
      
      DisplaySprite(sp_R64LOGO,224,146)
      
      If ElapsedMilliseconds() - c64displaytime > 4000 ; 4 seconds
        c64displaytime = ElapsedMilliseconds()  
        subpart = 4
      EndIf
      
    EndIf     
    
    If subpart = 4 ; display sabbat logo
      
      DisplaySprite(sp_SABBATLOGO,224,146)
      
      If ElapsedMilliseconds() - c64displaytime > 6000 ; 6 seconds
        c64displaytime = ElapsedMilliseconds()  
        subpart = 5
      EndIf
      
    EndIf      
    
    If subpart = 5 ; display ray's pumpkin
      
      DisplaySprite(sp_PUMPRAY,224,146) 
      
      If ElapsedMilliseconds() - c64displaytime > 4000 ; 4 seconds
        c64displaytime = ElapsedMilliseconds()  
        subpart = 6
      EndIf
      
    EndIf      
    
    If subpart = 6 ; display pennywise bitmap
      
      If pennywise = 0
        DisplaySprite(sp_CLOWNG,229,150)
        Delay(100)
      EndIf 
      
      If pennywise = 1
        DisplaySprite(sp_CLOWNG,220,140)
        Delay(100)
      EndIf 
      
      If pennywise = 2
        DisplaySprite(sp_CLOWNG,223,144)
        Delay(100)
      EndIf
      
      If pennywise = 3
        DisplaySprite(sp_CLOWN,224,146)
      EndIf  
      
      pennywise + 1
      If pennywise = 4
        pennywise = 3
      EndIf
      
      If ElapsedMilliseconds() - c64displaytime > 5000 ; 5 seconds
        c64displaytime = ElapsedMilliseconds()  
        subpart = 7
      EndIf
      
    EndIf      

    If subpart = 7 ; display main arkanix labs demo screen
      
      DisplaySprite(sp_ARKLOGO,224,146)
      
      AnimateC64Sprites() ; draw the c64 sprites from sabbat
      
      DrawScroller3() ; draw the hidden part scroller text on sabbat screen
      
      DisplaySprite(sp_BORDMASK,228,155) ; mask the c64 left and right borders with
      DisplaySprite(sp_BORDMASK,560,155) ; black strips. no sprites here in this demo!
      
    EndIf
    
    If subpart = 8 ; display the original sabbat silhouette that inspired old school demo iii!
      
      DisplaySprite(sp_SABSILL,224,146)

    EndIf   
    
    DrawScanlinesSmall() ; draw some small scanlines on the c64 screen if activated
    TransparentSpriteColor(sp_MONITOR, RGB(255,255,255)) ; set the monitor white screen as transparent
    DisplayTransparentSprite(sp_MONITOR,0,0)             ; display the monitor surround over everything
    
    ; when the hidden part is activated, the c64 boot screen is displayed above in subpart 1 and then
    ; the c64 assets are loaded below - the assets load when the c64 boot screen says 'loading'!
    ; clever, eh? ;)   once the c64 assets have loaded, the variable c64load is set to 0 so this little
    ; if/endif section is never called again.
    
    If c64load = 1
      FlipBuffers()
      LoadC64Assets()
      c64load = 0
    EndIf  
    
  EndIf
  
;}  
  
  
  FlipBuffers()                   ; flip to the second screen where drawing has been taking
                                  ; place to ensure there is no screen flicker  
  
  ExamineKeyboard()               ; see if the keyboard is being pressed
  
  
  KeyChecker()                    ; if the keyboard is pressed, call this procedure to check
                                  ; which key is pressed and what to do as a result of the keypress
  
  
  
Until KeyboardPushed(#PB_Key_Escape) ; if the 'esc' key is pressed, exit the loop and continue
                                      ; to 'exit program' below to tidy up and exit the code


;- EXIT PROGRAM ----------------------------------------------------     

; now the code is going to exit cleanly and tidy up after itself

flash + 1            ; add 1 to the flash variable to display a different flash silhouette

FadeToBlack()        ; flash the screen white with a halloween silhouette

; now lets set a while/wend loop to fade the music volume down from 100%
; to 0%.  we don't have to do this, it just sounds like a nicer
; ending than suddenly stopping the music!
;
; the slight delay inside the loop is so you hear the fade,
; otherwise it may happen so fast it sounds like a sudden stop anyway

If IsSound(m_CHOON)               ; if the main music is playing, fade that
  While vol > 0
    SoundVolume(m_CHOON, vol)
    vol = vol - 1
    Delay(10)
  Wend
  FreeSound(m_CHOON)
EndIf  

If IsSound(m_CHOON2)              ; if the hidden music is playing, fade that instead
  While vol > 0
    SoundVolume(m_CHOON2, vol)
    vol = vol - 1
    Delay(10)
  Wend
  FreeSound(m_CHOON2)
EndIf

; screen is blank and music has faded.  lets show a cosine website
; advert and play a laugh sound effect

ClearScreen(0)                                              ; ensure the screen is black
DisplaySprite(sp_COSURL,261,181)                            ; draw the cosine url
FlipBuffers()                                               ; show the image 
PlaySound(s_LAUGH, #PB_Sound_MultiChannel, 100)             ; play claypole laugh
Delay(6500)                                                 ; a 6.5 second delay

; lets be polite and clear out all the sounds, images, sprites and windows we've used
;
; the 'End' command at the, er, end does this automatically, we're
; just being very extra unnecessarily thorough here!

FreeSprite(#PB_All)         ; erase all sprites and free the memory
FreeSound(#PB_All)          ; erase the musics and free the memory
CloseWindow(#PB_All)        ; close the window and free the memory

End                         ; shut everything down and exit leaving no trace!

; IDE Options = PureBasic 5.60 (Windows - x64)
; Folding = AAAAA9
; EnableXP
; UseIcon = cos.ico
; Executable = 20181020-2158.exe
; Compiler = PureBasic 5.60 (Windows - x86)