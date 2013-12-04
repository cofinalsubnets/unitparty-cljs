(ns unitparty.unit.defs
  (:require [unitparty.unit :refer (uadd usub umul udiv uneg uexp)])
  (:use-macros [unitparty.unit.macros :only (defunits)]))

;; Unit definitions
;; Each unit is tagged with metadata containing its aliases and information
;; about it. We could potentially add significant digits for more precise
;; calculations, but this is a toy.
(let [unit     (fn [u] {{u 1} 1})
      constant (fn [c] {{} c})
      is       #(umul %2 (constant %1))
      shift    #(uadd %2 (constant %1))]

  (defunits *units*

    ;; SI base units
    (kilogram
     ["kilogram" "kilograms"]
      "SI base unit of mass."
     (unit :mass))
    (second' ;; actually overriding second seems to break clojurescript
     ["second" "seconds"]
      "SI base unit of time."
     (unit :time))
    (ampere
      ["ampere" "amperes" "amp" "amps"]
       "SI base unit of current."
      (unit :current))
    (meter
      ["meter" "meters" "metre" "meters"]
       "SI base unit of length."
      (unit :length))
    (candela
      ["candela" "candelas"]
       "SI base unit of luminous intensity."
      (unit :luminous-intensity))
    (mole
      ["mole" "moles" "mol" "mols"]
       "SI base unit of quantity."
      (unit :amount))
    (kelvin 
      ["kelvin" "kelvins"]
       "SI base unit of temperature."
      (unit :temperature))

    ;; SI derived units
    (pascal
      ["pascal" "pascals"]
       "SI derived unit of pressure."
      (umul kilogram (umul (uexp meter -1) (uexp second' -2))))
    (celsius
      ["celsius"]
       "SI derived unit of temperature."
      (shift -273.15 kelvin))
    (hertz
      ["hertz" "hz"]
       "SI derived unit of frequency."
      (uneg second'))
    (newton
      ["newton" "newtons"]
       "SI derived unit of force."
      (umul kilogram (udiv meter (uexp second' 2))))
    (joule
      ["joule" "joules"]
       "SI derived unit of energy."
      (umul newton meter))
    (watt
      ["watt" "watts"]
       "SI derived unit of power."
      (udiv joule second'))
    (coulomb
      ["coulomb" "coulombs"]
       "SI derived unit of electrical charge."
      (umul ampere second'))
    (volt
      ["volt" "volts"]
       "SI derived unit of electrical potential."
      (udiv watt ampere))
    (farad
      ["farad" "farads"]
       "SI derived unit of electrical capacitance."
      (udiv coulomb volt))
    (ohm
      ["ohm" "ohms"]
       "SI derived unit of electrical resistance."
      (udiv volt ampere))
    (siemens
      ["siemens" "mho" "mhos"]
       "SI derived unit of electrical conductance."
      (uneg ohm))
    (weber
      ["weber" "webers"]
       "SI derived unit of magnetic flux."
      (udiv joule ampere))
    (tesla
      ["tesla" "teslas"]
       "SI derived unit of magnetic flux density."
      (udiv weber (uexp meter 2)))
    (henry
      ["henry" "henrys" "henries"]
       "SI derived unit of inductance."
      (umul ohm second'))
    (lux
      ["lux"]
       "SI derived unit of illuminance."
      (udiv candela (uexp meter 2)))
    (gray
      ["gray" "grays"]
       "SI derived unit of absorbed dose of radiation."
      (udiv joule kilogram))
    (sievert
      ["sievert" "sieverts"]
       "SI derived unit of equivalent dose of radiation."
      gray)
    (katal
      ["katal" "katals"]
       "SI derived unit of catalytic activity."
      (udiv mole second'))
    (becquerel
      ["becquerel" "becquerels"]
       "SI derived unit of radioactivity."
      hertz)
    (lumen
      ["lumen" "lumens"]
       "SI derived unit of luminous flux."
      candela)
      
    ;; mass
    (gram
      ["gram" "grams" ]
       "Metric unit of mass."
      (is (/ 1 1000) kilogram))
    (pound
      ["pound" "pounds" "lb" "lbs"]
       "Customary unit of mass."
      (is 0.45359238 kilogram))
    (dalton
      ["dalton" "daltons" "amu" "amus"]
       "Unit of atomic mass."
      (is (* 1.66053886 (Math/pow 10 -27)) kilogram))
    (tonne
      ["tonne" "tonnes"]
       "SI unit of mass."
      (is 1000 kilogram))
    (carat
      ["carat" "carats"]
       "Unit of mass used for measuring gemstones. Originally derived from the weight of a carob seed."
      (is (/ 1 5000) kilogram))
    (grain
      ["grain" "grains"]
       "Apothecaries' unit of mass historically based on the weight of a seed of a cereal grain."
      (is 0.0648 gram))
    (scruple
      ["scruple" "scruples"]
       "Apothecaries' unit of mass."
      (is 20 grain))
    (dram
      ["dram" "drams" "drachm" "drachms"]
       "Apothecaries' unit of mass."
      (is 60 grain))
    (ounce
      ["ounce" "ounces" "oz"]
       "Customary unit of mass."
      (is (/ 1 16) pound))
    (stone
      ["stone"]
       "Imperial unit of mass common in Great Britain and Ireland."
      (is 14 pound))
    (shekel
      ["shekel" "shekels"]
       "Ancient Mesopotamian unit of mass (and currency)."
      (is 180 grain))
    (quarter
      ["quarter" "quarters"]
       "Imperial unit of mass."
      (is 2 stone))
    (hundredweight
      ["hundredweight" "hundredweights"]
       "Imperial unit of mass."
      (is 4 quarter))
    (wey
      ["wey" "weys"]
       "An archaic British unit of mass."
      (is 2 hundredweight))
    (ton
      ["ton" "tons"]
       "Imperial unit of mass."
      (is 20 hundredweight))
    (long-ton
      ["long-ton" "long-tons"]
       "Imperial unit of mass."
      ton)
    (short-ton
      ["short-ton" "short-tons"]
       "Imperial unit of mass."
      (is 2000 pound))
    (pennyweight
      ["pennyweight" "pennyweights"]
       "The mass of a Middle Ages British penny."
      (is 24 grain))
    (quintil
      ["quintil" "quintils"]
       "Unit of mass used used to measure French wine grape production."
      (is 100 ton))
    (earth
      ["earth" "earths"]
       "The mass of Earth."
      (is (* 5.97219 (Math/pow 10 24)) kilogram))
    (sun
      ["sun" "suns" "solar-mass" "solar-masses"]
       "One solar mass."
      (is 332946 earth))
    (mercury
      ["mercury" "mercuries"]
       "The mass of Mercury."
      (is 0.055 earth))
    (venus
      ["venus" "venuses" "veni"]
       "The mass of Venus."
      (is 0.815 earth))
    (moon
      ["moon" "moons"]
       "One lunar mass."
      (is 0.0123 earth))
    (mars
      ["mars" "marses"]
       "The mass of Mars."
      (is 0.107 earth))
    (jupiter
      ["jupiter" "jupiters"]
       "The mass of Jupiter."
      (is 317.8 earth))
    (saturn
      ["saturn" "saturns"]
       "The mass of Saturn."
      (is 95.16 earth))
    (uranus
      ["uranus" "uranuses" "urani"]
       "The mass of Uranus."
      (is 14.536 earth))
    (neptune
      ["neptune" "neptunes"]
       "The mass of Neptune."
      (is 17.147 earth))
    (pluto
      ["pluto" "plutos"]
       "The mass of Pluto, the best dwarf planet out there."
      (is 0.00218 earth))
    (libra
      ["libra" "librae"]
       "A Roman unit of mass."
      (is 328.9 gram))
    (deunx
      ["deunx"]
       "A Roman unit of mass."
      (is (/ 11 12) libra))
    (dextans
      ["dextans"]
       "A Roman unit of mass."
      (is (/ 5 6) libra))
    (dodrans
      ["dodrans"]
       "A Roman unit of mass."
      (is (/ 3 4) libra))
    (bes
      ["bes" "besis"]
       "A Roman unit of mass."
      (is (/ 2 3) libra))
    (septunx
      ["septunx"]
       "A Roman unit of mass."
      (is (/ 7 12) libra))
    (semis
      ["semis" "semissis"]
       "A Roman unit of mass."
      (is (/ 1 2) libra))
    (quincunx
      ["quincunx"]
       "A Roman unit of mass."
      (is (/ 5 12) libra))
    (triens
      ["triens"]
       "A Roman unit of mass."
      (is (/ 1 3) libra))
    (quadrans
      ["quadrans" "teruncius"]
       "A Roman unit of mass."
      (is (/ 1 4) libra))
    (sextans
      ["sextans"]
       "A Roman unit of mass."
      (is (/ 1 6) libra))
    (sescunx
      ["sescunx" "sescuncia"]
       "A Roman unit of mass."
      (is (/ 1 8) libra))
    (uncia
      ["uncia" "unciae"]
       "A Roman unit of mass."
      (is (/ 1 12) libra))
    (siliqua
      ["siliqua" "siliquae"]
       "A Roman unit of mass."
      (is (/ 1 144) uncia))
    (obolus
      ["obolus" "oboli"]
       "A Roman unit of mass."
      (is (/ 1 48) uncia))
    (scrupulum
      ["scrupulum" "scrupula"]
       "A Roman unit of mass."
      (is (/ 1 24) uncia))
    (semisextula
      ["semisextula" "semisextulae"]
       "A Roman unit of mass."
      (is (/ 1 12) uncia))
    (sextula
      ["sextula" "sextulae"]
       "A Roman unit of mass."
      (is (/ 1 6) uncia))
    (sicilicius
      ["sicilicius" "sicilicii"]
       "A Roman unit of mass."
      (is (/ 1 4) uncia))
    (duella
      ["duella" "duellae"]
       "A Roman unit of mass."
      (is (/ 1 3) uncia))
    (semuncia
      ["semuncia" "semunciae"]
       "A Roman unit of mass."
      (is (/ 1 2) uncia))
    

    ;; volume
    (liter
      ["litre" "liter" "litres" "liters"]
       "Metric unit of volume."
      (is 0.001 (uexp meter 3)))
    (fluid-ounce
      ["fluid-ounce" "fluid-ounces" "floz"]
       "An Imperial and Customary unit of volume."
      (is 0.0284130625 liter))
    (gill
      ["gill" "gills" "gi"]
       "An Imperial and Customary unit of volume."
      (is 5 fluid-ounce))
    (pint
      ["pint" "pints"]
       "An Imperial and Customary unit of volume."
      (is 4 gill))
    (quart
      ["quart" "quarts"]
       "An Imperial and Customary unit of volume."
      (is 2 pint))
    (gallon
      ["gallon" "gallons" "ga"]
       "An Imperial and Customary unit of volume."
      (is 4 quart))
    (fluid-dram
      ["fluid-dram" "fluid-drams" "fluid-drachm" "fluid-drachms"]
       "Apothecaries' unit of volume."
      (is (/ 1 8) fluid-ounce))
    (fluid-scruple
      ["fluid-scruple" "fluid-scruples"]
       "Apothecaries' unit of volume."
      (is (/ 1 3) fluid-dram))
    (minim
      ["minim" "minims"]
       "Apothecaries' unit of volume."
      (is (/ 1 20) fluid-scruple))
    (teaspoon
      ["teaspoon" "teaspoons" "tsp"]
       "A common culinary unit of volume."
      (is 1 fluid-dram))
    (tablespoon
      ["tablespoon" "tablespoons" "tbsp"]
       "A common culinary unit of volume."
      (is 3 teaspoon))
    (dessertspoon
      ["dessertspoon" "dessertspoons" "dstspn"]
       "A less common culinary unit of volume."
      (is 2 teaspoon))
    (tun
      ["tun" "tuns"]
       "An English wine cask unit."
      (is 252 gallon))
    (pipe
      ["pipe" "pipes" "butt" "butts"]
       "An English wine cask unit."
      (is (/ 1 2) tun))
    (puncheon
      ["puncheon" "puncheons" "tertian" "tertians"]
       "An English wine cask unit."
      (is (/ 1 3) tun))
    (hogshead
      ["hogshead" "hogsheads"]
       "An English wine cask unit."
      (is (/ 1 4) tun))
    (tierce
      ["tierce" "tierces"]
       "An English wine cask unit."
      (is (/ 1 6) tun))
    (barrel
      ["barrel" "barrels"]
       "An English wine cask unit."
      (is (/ 1 8) tun))
    (rundlet
      ["rundlet" "rundlets"]
       "An English wine cask unit."
      (is (/ 1 14) tun))
    (kilderkin
      ["kilderkin" "kilderkins"]
       "An English brewery cask unit."
      (is 83.18 liter))
    (firkin
      ["firkin" "firkins"]
       "An English brewery cask unit."
      (is (/ 1 2) kilderkin))
    (pin
      ["pin" "pins"]
       "An English brewery cask unit."
      (is (/ 1 2) firkin))
    (peck
      ["peck" "pecks"]
       "An Imperial and Customary unit of volume."
      (is 2 gallon))
    (kenning
      ["kenning" "kennings"]
       "An Imperial and Customary unit of volume."
      (is 2 peck))
    (bushel
      ["bushel" "bushels"]
       "An Imperial and Customary unit of volume."
      (is 2 kenning))
    (hobbit
      ["hobbit" "hobbits"]
       "A Welsh unit of volume (and sometimes mass). Really!"
      (is 2.5 bushel))
    (kochliarion
      ["kochliarion" "kochliaria"]
       "An ancient Greek unit of volume."
      (is 0.0045 liter))
    (xeme
      ["xeme"]
       "An ancient Greek unit of volume."
      (is 2 kochliarion))
    (mustron
      ["mustron" "mustra"]
       "An ancient Greek unit of volume."
      (is 2.5 kochliarion))
    (konche
      ["konche"]
       "An ancient Greek unit of volume."
      (is 5 kochliarion))
    (kyathos
      ["kyathos" "kyathoi"]
       "An ancient Greek unit of volume."
      (is 10 kochliarion))
    (oxybathon
      ["oxybathon" "oxybatha"]
       "An ancient Greek unit of volume."
      (is 1.5 kyathos))
    (tetarton
      ["tetarton" "tetarta"]
       "An ancient Greek unit of volume."
      (is 3 kyathos))
    (kotyle
      ["kotyle"]
       "An ancient Greek unit of volume."
      (is 6 kyathos))
    (xestes
      ["xestes"]
       "An ancient Greek unit of liquid volume."
      (is 12 kyathos))
    (chous
      ["chous" "choes"]
       "An ancient Greek unit of liquid volume."
      (is 72 kyathos))
    (keramion
      ["keramion" "keramia"]
       "An ancient Greek unit of liquid volume."
      (is 8 chous))
    (metretes
      ["metretes"]
       "An ancient Greek unit of liquid volume."
      (is 12 chous))
    (choinix
      ["choinix" "choinikes"]
       "An ancient Greek unit of dry volume."
      (is 24 kyathos))
    (hemiekton
      ["hemiekton" "hemiekta"]
       "An ancient Greek unit of dry volume."
      (is 4 choinix))
    (hecteus
      ["hecteus"]
       "An ancient Greek unit of dry volume."
      (is 8 choinix))
    (medimnos
      ["medimnos"]
       "An ancient Greek unit of dry volume."
      (is 48 choinix))
    (sextarius
      ["sextarius" "sextarii"]
       "The basis for ancient Roman units of volume."
      (is 0.546 liter))
    (ligula
      ["ligula" "ligulae"]
       "A Roman unit of volume."
      (is (/ 1 48) sextarius))
    (cyathus
      ["cyathus" "cyathi"]
       "A Roman unit of volume."
      (is (/ 1 12) sextarius))
    (acetabulum
      ["acetabulum" "acetabula"]
       "A Roman unit of volume."
      (is (/ 1 8) sextarius))
    (quartarius
      ["quartarius" "quartarii"]
       "A Roman unit of volume."
      (is (/ 1 4) sextarius))
    (hemina
      ["hemina" "heminae"]
       "A Roman unit of volume."
      (is (/ 1 2) sextarius))
    (congius
      ["congius" "congii"]
       "A Roman unit of liquid volume."
      (is 6 sextarius))
    (urna
      ["urna" "urnae"]
       "A Roman unit of liquid volume."
      (is 4 congius))
    (amphora-quadrantal
      ["amphora-quadrantal"]
       "A Roman unit of liquid volume."
      (is 8 congius))
    (culeus
      ["culeus" "culei"]
       "A Roman unit of liquid volume."
      (is 160 congius))
    (semimodius
      ["semimodius" "semimodii"]
       "A Roman unit of dry volume."
      (is 8 sextarius))
    (modius
      ["modius" "modii"]
       "A Roman unit of dry volume."
      (is 16 sextarius))


    ;; length
    (planck-length
      ["planck-length" "planck-lengths"]
       "An appallingly tiny distance significant in theoretical physics."
      (is (* 1.616199 (Math/pow 10 -35)) meter))
    (foot
      ["foot" "feet" "ft"]
       "An Imperial and Customary unit of length."
      (is 0.3048 meter))
    (inch
      ["inch" "inches"]
       "An Imperial and Customary unit of length."
      (is (/ 1 12) foot))
    (yard
      ["yard" "yards"]
       "An Imperial and Customary unit of length."
      (is 3 foot))
    (chain
      ["chain" "chains"]
       "An Imperial and Customary unit of length."
      (is 22 yard))
    (link
      ["link" "links"]
       "An Imperial and Customary unit of length."
      (is (/ 1 100) chain))
    (rod
      ["rod" "rods"]
       "An Imperial and Customary unit of length."
      (is (/ 1 4) chain))
    (furlong
      ["furlong" "furlongs"]
       "An Imperial and Customary unit of length."
      (is 10 chain))
    (mile
      ["mile" "miles"]
       "An Imperial and Customary unit of length."
      (is 8 furlong))
    (league
      ["league" "leagues"]
       "An Imperial and Customary unit of length."
      (is 3 mile))
    (fathom
      ["fathom" "fathoms"]
       "A nautical unit of length."
      (is 2 yard))
    (cable
      ["cable" "cables"]
       "A nautical unit of length."
      (is 100 fathom))
    (nautical-mile
      ["nautical-mile" "nautical-miles"]
       "A nautical unit of length."
      (is 10 cable))
    (cubit
      ["cubit" "cubits"]
       "An ancient unit of length, representing the distance from the elbow to the tip of the middle finger."
      (is 18 inch))
    (ell
      ["ell" "ells"]
       "A unit of length used in English tailoring; at other times, identical to the cubit."
      (is 45 inch))
    (finger
      ["finger" "fingers"]
       "A unit of length based on the width of a human finger."
      (is (/ 7 8) inch))
     (fermi
       ["fermi" "fermis"]
        "An SI unit of length equivalent to one femtometer. Named for Enrico Fermi."
       (is (Math/pow 10 -15) meter))
     (micron
       ["micron" "microns"]
        "An SI unit of length equivalent to one micrometer."
       (is (Math/pow 10 -6) meter))
    (thou
      ["thou" "thous"]
       "A unit of length used in engineering and manufacturing."
      (is (/ 1 1000) inch))
    (mil
      ["mil" "mils"]
       "A unit of length used in engineering and manufacturing."
      (is (/ 1 1000) inch))
    (hand
      ["hand" "hands"]
       "A unit of length currently used for measuring the height of horses."
      (is 0.1016 meter))

    (angstrom
      ["angstrom" "angstroms"]
       "An atomic-scale unit of length."
      (is (Math/pow 10 -10) meter))
    (light-year
      ["light-year" "light-years"]
       "The distance traveled by light in a vacuum in one Julian year."
      (is (* 9.4607 (Math/pow 10 15)) meter))
    (parsec
      ["parsec" "parsecs"]
       "PARallax of one arcSECond. A unit of distance used in astronomy."
      (is 3.26156 light-year))
    (astronomical-unit
      ["astronomical-unit" "astronomical-units"]
       "The distance from the Earth to the Sun."
      (is 149597870700 meter))
    (barleycorn
      ["barleycorn" "barleycorns"]
       "An archaic Anglo-Saxon unit of length."
      (is (/ 1 3) inch))
    (palm
      ["palm" "palms"]
       "An archaic Anglo-Saxon unit of length."
      (is 0.0726 meter))
    (shaftment
      ["shaftment" "shaftments"]
       "An archaic Anglo-Saxon unit of length."
      (is 2 palm))
    (bahar
      ["bahar" "bahars"]
       "An obsolete Persian unit of length."
      (is 0.0325 meter))
    (cana
      ["cana" "canas"]
       "An archaic unit of length used in the Crown of Aragon."
      (is 1.5708 meter))
    (smoot
      ["smoot" "smoots"]
       "The height of Oliver R. Smoot in 1958."
      (is 67 inch))
    (jow
      ["jow" "jows"]
       "An obsolete Indian unit of length."
      (is (/ 1 4) inch))
    (daktylos
      ["daktylos" "daktyloi"]
       "An ancient Greek unit of length. Supposed to represent the width of one finger."
      (is 0.76 inch))
    (kondylos
      ["kondylos" "kondyloi"]
       "An ancient Greek unit of length."
      (is 2 daktylos))
    (doron
      ["doron" "dora"]
       "An ancient Greek unit of length. Supposed to represent the width of one palm."
      (is 4 daktylos))
    (palaiste
      ["palaiste"]
       "An ancient Greek unit of length."
      doron)
    (hemipodion
      ["hemipodion" "hemipodia"]
       "An ancient Greek unit of length."
      (is 8 daktylos))
    (dichas
      ["dichas"]
       "An ancient Greek unit of length."
      hemipodion)
    (lichas
      ["lichas"]
       "An ancient Greek unit of length."
      (is 10 daktylos))
    (orthodoron
      ["orthodoron" "orthodora"]
       "An ancient Greek unit of length."
      (is 11 daktylos))
    (spithame
      ["spithame"]
       "An ancient Greek unit of length. Supposed to represent the width of all of one person's fingers."
      (is 12 daktylos))
    (pous
      ["pous" "podes"]
       "An ancient Greek unit of length. Supposed to represent the length of one foot."
      (is 16 daktylos))
    (pygme
      ["pygme"]
       "An ancient Greek unit of length."
      (is 18 daktylos))
    (pygon
      ["pygon" "pyga"]
       "An ancient Greek unit of length."
      (is 20 daktylos))
    (pechys
      ["pechys"]
       "An ancient Greek unit of length."
      (is 24 daktylos))
    (orgyia
      ["orgyia"]
       "An ancient Greek unit of length."
      (is 6 pous))
    (dekapous
      ["dekapous" "dekapodes"]
       "An ancient Greek unit of length."
      (is 10 pous))
    (hamma
      ["hamma"]
       "An ancient Greek unit of length."
      (is 60 pous))
    (plethron
      ["plethron" "plethra"]
       "An ancient Greek unit of length."
      (is 100 pous))
    (stadion
      ["stadion" "stadia"]
       "An ancient Greek unit of length."
      (is 600 pous))
    (diaulos
      ["diaulos"]
       "An ancient Greek unit of length."
      (is 2 stadion))
    (hippikon
      ["hippikon" "hippika"]
       "An ancient Greek unit of length."
      (is 4 stadion))
    (milion
      ["milion" "milia"]
       "An ancient Greek unit of length."
      (is 8 stadion))
    (dolichos
      ["dolichos"]
       "An ancient Greek unit of length."
      (is 12 stadion))
    (parasanges
      ["parasanges"]
       "An ancient Greek unit of length."
      (is 30 stadion))
    (schoinos
      ["schoinos"]
       "An ancient Greek unit of length."
      (is 40 stadion))
    (stage
      ["stage" "stages"]
       "An ancient Greek unit of length."
      (is 160 stadion))

    ;; area
    (perch
      ["perch" "perches"]
       "An Imperial and Customary unit of area."
      (uexp rod 2))
    (rood
      ["rood" "roods"]
       "An Imperial and Customary unit of area."
      (umul furlong rod))
    (acre
      ["acre" "acres"]
       "An Imperial and Customary unit of area."
      (umul furlong chain))
    (section
      ["section" "sections"]
       "A unit of area used in surveying."
      (is 640 acre))
    (township
      ["township" "townships"]
       "A unit of area used in surveying."
      (is 36 section))
    (are
      ["are" "ares"]
       "An obsolete metric unit of area."
      (is 100 (uexp meter 2)))
    (decare
      ["decare" "decares"]
       "An obsolete metric unit of area."
      (is 10 are))
    (hectare
      ["hectare" "hectares"]
       "A metric unit of area."
      (is 100 are))
    (dunam
      ["dunam" "dunams"]
       "An Ottoman unit of area, representing the amount of land that can be plowed by one person in one day."
      decare)
    (marabba
      ["marabba" "marabbas"]
       "A Pakistani unit of area."
      (is 25 acre))
    (jareeb
      ["jareeb" "jareebs"]
       "A Pakistani unit of area."
      (is (/ 1 10) acre))
    (kanee
      ["kanee" "kanees"]
       "A Pakistani unit of area."
      (is 4 jareeb))
    (guntha
      ["guntha" "gunthas"]
       "A Pakistani unit of area."
      (is (/ 1 4) jareeb))
    (anna
      ["anna" "annas"]
       "A Pakistani unit of area."
      (is (/ 1 6) guntha))
    (barn
      ["barn" "barns"]
       "A measure of area used in particle physics."
      (is (Math/pow 10 -28) (uexp meter 2)))

    ;; pressure
    (bar
      ["bar" "bars"]
       "A non-SI unit of pressure."
      (is 100000 pascal))
    (barye
      ["barye" "baryes"]
       "A non-SI unit of pressure."
      (is (/ 1 10) pascal))
    (atmosphere
      ["atmosphere" "atmosphere" "atm"]
       "One standard Earth atmosphere."
      (is 101325 pascal))
    (torr
      ["torr" "torrs"]
       "A non-SI unit of pressure."
      (is (/ 1 760) atmosphere))
    (mmhg
      ["mmhg" "millimeters-of-mercury" "millimetres-of-mercury"]
       "A non-SI unit of pressure."
      (is 133.322387415  pascal))
   
    ;; energy
    (erg
      ["erg" "ergs"]
       "A non-SI unit of energy."
      (is (Math/pow 10 -7) joule))
    (electron-volt
      ["electron-volt" "electron-volt"]
       "The energy gained or lost by a charge of one electron when moved across a potential difference of one volt."
      (is (* 1.602176565 (Math/pow 10 -19)) joule))
    (btu
      ["btu" "btus" "british-thermal-unit" "british-thermal-units"]
       "The amount of energy needed to heat or cool one pound of water by one degree Fahrenheit."
      (is 1055 joule))
    (calorie
      ["calorie" "calories"]
       "The amount of energy needed to heat or cool one gram of water by one degree Celsius."
      (is 4.184 joule))

    ;; power
    (horsepower
      ["horsepower"]
       "A non-SI unit of power."
      (is 745.69987158 watt))

    ;; temperature
    (rankine
      ["rankine" "degree-rankine" "degrees-rankine"]
       "A non-SI unit of temperature. Rankine is to Fahrenheit as Kelvin is to Celsius."
      (is (/ 5 9) kelvin))

    (fahrenheit
      ["fahrenheit" "degree-fahrenheit" "degrees-fahrenheit"]
       "A non-SI unit of temperature."
      (shift -459.67 rankine))


    ;; units related to radiation and radioactivity
    (roentgen
      ["roentgen" "roentgens"]
       "Unit of X-ray and gamma ray kerma."
      (is 0.000258 (udiv coulomb kilogram)))
    (rem'
      ["rem" "rems"]
       "Roentgen Equivalent in Mammal. A unit of effective dose of radiation."
      (is 0.01 sievert))
    (rad
      ["rad" "rads"]
       "Unit of absorbed dose of radiation."
      (is 0.01 gray))

    ;; force
    (pound-force
      ["pound-force" "pounds-force"]
       "A non-SI unit of force."
      (is 32.174049 (umul pound (udiv foot (uexp second' 2)))))
    (dyne
      ["dyne" "dynes"]
       "A CGS unit of force."
      (is (Math/pow 10 -5) newton))
    (kip
      ["kip" "kips"]
      "A non-SI unit of force."
      (is 1000 pound-force))
    (poundal
      ["poundal" "poundals"]
      "An Imperial unit of force."
      (umul pound (udiv foot (uexp second' 2))))

    ;; mass again
    (slug
      ["slug" "slugs"]
      "An Imperial unit of mass."
      (umul pound-force (udiv foot (uexp second' 2))))

    ;; time
    (minute
      ["minute" "minutes"]
      "Common unit of time."
      (is 60 second'))
    (hour
      ["hour" "hours"]
      "Common unit of time."
      (is 60 minute))
    (day
      ["day" "days"]
      "Common unit of time."
      (is 24 hour))
    (week
      ["week" "weeks"]
      "Common unit of time."
      (is 7 day))
    (weekend
      ["weekend" "weekends"]
      "~partytime~"
      (is 2 day))
    (fortnight
      ["fortnight" "fortnights"]
      "Common unit of time (outside of North America)."
      (is 2 week))
    (year
      ["year" "years"]
      "Julian year, used in astronomy."
      (is 365.25 day))
    (decade
      ["decade" "decades"]
      "Ten Julian years."
      (is 10 year))
    (century
      ["century" "centuries"]
      "Ten Julian decades."
      (is 10 decade))
    (millennium
      ["millennium" "millennia"]
      "Ten Julian centuries."
      (is 10 century))
    (planck-time
      ["planck-time" "planck-times"]
      "Time taken for light in a vacuum to travel 1 Planck length."
      (is (* 5.39106 (Math/pow 10 -44)) second'))
    (svedberg
      ["svedberg" "svedbergs"]
      "Unit of time used to measure sedimentation rate."
      (is (Math/pow 10 -13) second'))
    (helek
      ["helek" "halakim"]
      "A unit of time used in the calculation of the Hebrew calendar."
      (is (/ 10 3) second'))
    (ke
      ["ke"]
      "A Chinese unit of time."
      (is 15 minute))
    (shake
      ["shake" "shakes"]
       "A unit of time used in nuclear physics."
      (is (Math/pow 10 -8) second'))

    ;; acceleration
    (standard-gravity
      ["standard-gravity" "standard-gravities"]
      "The acceleration due to gravity experienced by a free-falling object in a vacuum near the surface of the Earth."
      (is 9.80665 (udiv meter (uexp second' 2))))
    (gal
      ["gal" "gals" "galileo" "galileos"]
       "Unit of acceleration. Named for Galileo Galilei."
      (is (/ 1 100) (udiv meter (uexp second' 2))))

    ;; nothing else is like a sverdrup <3
    (sverdrup
      ["sverdrup" "sverdrups"]
       "Unit of rate of volume transport. Used in oceanography."
      (is 1000000 (udiv (uexp meter 3) second')))))

