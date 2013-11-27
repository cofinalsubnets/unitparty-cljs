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

  (defunits *units* *unique-units*

    ;; SI base units
    (kilogram
     {:names ["kilogram" "kilograms"]
      :info "SI base unit of mass."}
     (unit :mass))
    (second' ;; actually overriding second seems to break clojurescript
     {:names ["second" "seconds"]
      :info "SI base unit of time."}
     (unit :time))
    (ampere
      {:names ["ampere" "amperes" "amp" "amps"]
       :info "SI base unit of current."}
      (unit :current))
    (meter
      {:names ["meter" "meters" "metre" "meters"]
       :info "SI base unit of length."}
      (unit :length))
    (candela
      {:names ["candela" "candelas"]
       :info "SI base unit of luminous intensity."}
      (unit :luminous-intensity))
    (mole
      {:names ["mole" "moles" "mol" "mols"]
       :info "SI base unit of quantity."}
      (unit :amount))
    (kelvin 
      {:names ["kelvin" "kelvins"]
       :info "SI base unit of temperature."}
      (unit :temperature))

    ;; SI derived units
    (pascal
      {:names ["pascal" "pascals"]
       :info  "SI derived unit of pressure."}
      (umul kilogram (umul (uexp meter -1) (uexp second' -2))))
    (celsius
      {:names ["celsius"]
       :info  "SI derived unit of temperature."}
      (shift -273.15 kelvin))
    (hertz
      {:names ["hertz" "hz"]
       :info  "SI derived unit of frequency."}
      (uneg second'))
    (newton
      {:names ["newton" "newtons"]
       :info  "SI derived unit of force."}
      (umul kilogram (udiv meter (uexp second' 2))))
    (joule
      {:names ["joule" "joules"]
       :info  "SI derived unit of energy."}
      (umul newton meter))
    (watt
      {:names ["watt" "watts"]
       :info  "SI derived unit of power."}
      (udiv joule second'))
    (coulomb
      {:names ["coulomb" "coulombs"]
       :info  "SI derived unit of electrical charge."}
      (umul ampere second'))
    (volt
      {:names ["volt" "volts"]
       :info  "SI derived unit of electrical potential."}
      (udiv watt ampere))
    (farad
      {:names ["farad" "farads"]
       :info  "SI derived unit of electrical capacitance."}
      (udiv coulomb volt))
    (ohm
      {:names ["ohm" "ohms"]
       :info  "SI derived unit of electrical resistance."}
      (udiv volt ampere))
    (siemens
      {:names ["siemens" "mho" "mhos"]
       :info  "SI derived unit of electrical conductance."}
      (uneg ohm))
    (weber
      {:names ["weber" "webers"]
       :info  "SI derived unit of magnetic flux."}
      (udiv joule ampere))
    (tesla
      {:names ["tesla" "teslas"]
       :info  "SI derived unit of magnetic flux density."}
      (udiv weber (uexp meter 2)))
    (henry
      {:names ["henry" "henrys" "henries"]
       :info  "SI derived unit of inductance."}
      (umul ohm second'))
    (lux
      {:names ["lux"]
       :info  "SI derived unit of illuminance."}
      (udiv candela (uexp meter 2)))
    (gray
      {:names ["gray" "grays"]
       :info  "SI derived unit of absorbed dose of radiation."}
      (udiv joule kilogram))
    (sievert
      {:names ["sievert" "sieverts"]
       :info  "SI derived unit of equivalent dose of radiation."}
      gray)
    (katal
      {:names ["katal" "katals"]
       :info  "SI derived unit of catalytic activity."}
      (udiv mole second'))
    (becquerel
      {:names ["becquerel" "becquerels"]
       :info  "SI derived unit of radioactivity."}
      hertz)
    (lumen
      {:names ["lumen" "lumens"]
       :info  "SI derived unit of luminous flux."}
      candela)
      
    ;; mass
    (gram
      {:names ["gram" "grams" ]
       :info "Metric unit of mass."}
      (is (/ 1 1000) kilogram))
    (pound
      {:names ["pound" "pounds" "lb" "lbs"]
       :info "Customary unit of mass."}
      (is 0.45359238 kilogram))
    (dalton
      {:names ["dalton" "daltons" "amu" "amus"]
       :info "Unit of atomic mass."}
      (is (* 1.66053886 (Math/pow 10 -27)) kilogram))
    (tonne
      {:names ["tonne" "tonnes"]
       :info "SI unit of mass."}
      (is 1000 kilogram))
    (carat
      {:names ["carat" "carats"]
       :info "Unit of mass used for measuring gemstones. Originally derived from the weight of a carob seed."}
      (is (/ 1 5000) kilogram))
    (grain
      {:names ["grain" "grains"]
       :info "Apothecaries' unit of mass historically based on the weight of a seed of a cereal grain."}
      (is 0.0648 gram))
    (scruple
      {:names ["scruple" "scruples"]
       :info "Apothecaries' unit of mass."}
      (is 20 grain))
    (dram
      {:names ["dram" "drams" "drachm" "drachms"]
       :info "Apothecaries' unit of mass."}
      (is 60 grain))
    (ounce
      {:names ["ounce" "ounces" "oz"]
       :info "Customary unit of mass."}
      (is (/ 1 16) pound))
    (stone
      {:names ["stone"]
       :info "Imperial unit of mass common in Great Britain and Ireland."}
      (is 14 pound))
    (shekel
      {:names ["shekel" "shekels"]
       :info "Ancient Mesopotamian unit of mass (and currency)."}
      (is 180 grain))
    (quarter
      {:names ["quarter" "quarters"]
       :info "Imperial unit of mass."}
      (is 2 stone))
    (hundredweight
      {:names ["hundredweight" "hundredweights"]
       :info "Imperial unit of mass."}
      (is 4 quarter))
    (wey
      {:names ["wey" "weys"]
       :info  "An archaic British unit of mass."}
      (is 2 hundredweight))
    (ton
      {:names ["ton" "tons"]
       :info "Imperial unit of mass."}
      (is 20 hundredweight))
    (pennyweight
      {:names ["pennyweight" "pennyweights"]
       :info "The mass of a Middle Ages British penny."}
      (is 24 grain))
    (quintil
      {:names ["quintil" "quintils"]
       :info "Unit of mass used used to measure French wine grape production."}
      (is 100 ton))
    (earth
      {:names ["earth" "earths"]
       :info "The mass of Earth."}
      (is (* 5.97219 (Math/pow 10 24)) kilogram))
    (sun
      {:names ["sun" "suns" "solar-mass" "solar-masses"]
       :info "One solar mass."}
      (is 332946 earth))
    (mercury
      {:names ["mercury" "mercuries"]
       :info "The mass of Mercury."}
      (is 0.055 earth))
    (venus
      {:names ["venus" "venuses" "veni"]
       :info "The mass of Venus."}
      (is 0.815 earth))
    (moon
      {:names ["moon" "moons"]
       :info "One lunar mass."}
      (is 0.0123 earth))
    (mars
      {:names ["mars" "marses"]
       :info "The mass of Mars."}
      (is 0.107 earth))
    (jupiter
      {:names ["jupiter" "jupiters"]
       :info  "The mass of Jupiter."}
      (is 317.8 earth))
    (saturn
      {:names ["saturn" "saturns"]
       :info  "The mass of Saturn."}
      (is 95.16 earth))
    (uranus
      {:names ["uranus" "uranuses" "urani"]
       :info  "The mass of Uranus."}
      (is 14.536 earth))
    (neptune
      {:names ["neptune" "neptunes"]
       :info  "The mass of Neptune."}
      (is 17.147 earth))
    (pluto
      {:names ["pluto" "plutos"]
       :info  "The mass of Pluto, the best dwarf planet out there."}
      (is 0.00218 earth))

    ;; volume
    (liter
      {:names ["litre" "liter" "litres" "liters"]
       :info "Metric unit of volume."}
      (is 0.001 (uexp meter 3)))
    (fluid-ounce
      {:names ["fluid-ounce" "fluid-ounces" "floz"]
       :info  "An Imperial and Customary unit of volume."}
      (is 0.0284130625 liter))
    (gill
      {:names ["gill" "gills" "gi"]
       :info  "An Imperial and Customary unit of volume."}
      (is 5 fluid-ounce))
    (pint
      {:names ["pint" "pints"]
       :info  "An Imperial and Customary unit of volume."}
      (is 4 gill))
    (quart
      {:names ["quart" "quarts"]
       :info  "An Impoerial and Customary unit of volume."}
      (is 2 pint))
    (gallon
      {:names ["gallon" "gallons" "ga"]
       :info  "An Imperial and Customary unit of volume."}
      (is 4 quart))
    (fluid-dram
      {:names ["fluid-dram" "fluid-drams" "fluid-drachm" "fluid-drachms"]
       :info  "Apothecaries' unit of volume."}
      (is (/ 1 8) fluid-ounce))
    (fluid-scruple
      {:names ["fluid-scruple" "fluid-scruples"]
       :info  "Apothecaries' unit of volume."}
      (is (/ 1 3) fluid-dram))
    (minim
      {:names ["minim" "minims"]
       :info  "Apothecaries' unit of volume."}
      (is (/ 1 20) fluid-scruple))
    (teaspoon
      {:names ["teaspoon" "teaspoons" "tsp"]
       :info  "A common culinary unit of volume."}
      (is 1 fluid-dram))
    (tablespoon
      {:names ["tablespoon" "tablespoons" "tbsp"]
       :info  "A common culinary unit of volume."}
      (is 3 teaspoon))
    (dessertspoon
      {:names ["dessertspoon" "dessertspoons" "dstspn"]
       :info  "A less common culinary unit of volume."}
      (is 2 teaspoon))
    (tun
      {:names ["tun" "tuns"]
       :info  "An English wine cask unit."}
      (is 252 gallon))
    (pipe
      {:names ["pipe" "pipes" "butt" "butts"]
       :info  "An English wine cask unit."}
      (is (/ 1 2) tun))
    (puncheon
      {:names ["puncheon" "puncheons" "tertian" "tertians"]
       :info  "An English wine cask unit."}
      (is (/ 1 3) tun))
    (hogshead
      {:names ["hogshead" "hogsheads"]
       :info  "An English wine cask unit."}
      (is (/ 1 4) tun))
    (tierce
      {:names ["tierce" "tierces"]
       :info  "An English wine cask unit."}
      (is (/ 1 6) tun))
    (barrel
      {:names ["barrel" "barrels"]
       :info  "An English wine cask unit."}
      (is (/ 1 8) tun))
    (rundlet
      {:names ["rundlet" "rundlets"]
       :info  "An English wine cask unit."}
      (is (/ 1 14) tun))
    (kilderkin
      {:names ["kilderkin" "kilderkins"]
       :info  "An English brewery cask unit."}
      (is 83.18 liter))
    (firkin
      {:names ["firkin" "firkins"]
       :info  "An English brewery cask unit."}
      (is (/ 1 2) kilderkin))
    (pin
      {:names ["pin" "pins"]
       :info  "An English brewery cask unit."}
      (is (/ 1 2) firkin))
    (peck
      {:names ["peck" "pecks"]
       :info  "An Imperial and Customary unit of volume."}
      (is 2 gallon))
    (kenning
      {:names ["kenning" "kennings"]
       :info  "An Imperial and Customary unit of volume."}
      (is 2 peck))
    (bushel
      {:names ["bushel" "bushels"]
       :info  "An Imperial and Customary unit of volume."}
      (is 2 kenning))
    (hobbit
      {:names ["hobbit" "hobbits"]
       :info  "A Welsh unit of volume (and sometimes mass). Really!."}
      (is 2.5 bushel))

    ;; length
    (planck-length
      {:names ["planck-length" "planck-lengths"]
       :info  "An appallingly tiny distance significant in theoretical physics."}
      (is (* 1.616199 (Math/pow 10 -35)) meter))
    (foot
      {:names ["foot" "feet" "ft"]
       :info  "An Imperial and Customary unit of length."}
      (is 0.3048 meter))
    (inch
      {:names ["inch" "inches"]
       :info  "An Imperial and Customary unit of length."}
      (is (/ 1 12) foot))
    (yard
      {:names ["yard" "yards"]
       :info  "An Imperial and Customary unit of length."}
      (is 3 foot))
    (chain
      {:names ["chain" "chains"]
       :info  "An Imperial and Customary unit of length."}
      (is 22 yard))
    (link
      {:names ["link" "links"]
       :info  "An Imperial and Customary unit of length."}
      (is (/ 1 100) chain))
    (rod
      {:names ["rod" "rods"]
       :info "An Imperial and Customary unit of length."}
      (is (/ 1 4) chain))
    (furlong
      {:names ["furlong" "furlongs"]
       :info  "An Imperial and Customary unit of length."}
      (is 10 chain))
    (mile
      {:names ["mile" "miles"]
       :info  "An Imperial and Customary unit of length."}
      (is 8 furlong))
    (league
      {:names ["league" "leagues"]
       :info  "An Imperial and Customary unit of length."}
      (is 3 mile))
    (fathom
      {:names ["fathom" "fathoms"]
       :info  "A nautical unit of length."}
      (is 2 yard))
    (cable
      {:names ["cable" "cables"]
       :info  "A nautical unit of length."}
      (is 100 fathom))
    (nautical-mile
      {:names ["nautical-mile" "nautical-miles"]
       :info  "A nautical unit of length."}
      (is 10 cable))
    (cubit
      {:names ["cubit" "cubits"]
       :info  "An ancient unit of length, representing the distance from the elbow to the tip of the middle finger."}
      (is 18 inch))
    (ell
      {:names ["ell" "ells"]
       :info  "A unit of length used in English tailoring; at other times, identical to the cubit."}
      (is 45 inch))
    (finger
      {:names ["finger" "fingers"]
       :info  "A unit of length based on the width of a human finger."}
      (is (/ 7 8) inch))
     (fermi
       {:names ["fermi" "fermis"]
        :info  "An SI unit of length equivalent to one femtometer. Named for Enrico Fermi."}
       (is (Math/pow 10 -15) meter))
     (micron
       {:names ["micron" "microns"]
        :info  "An SI unit of length equivalent to one micrometer."}
       (is (Math/pow 10 -6) meter))
    (thou
      {:names ["thou" "thous"]
       :info  "A unit of length used in engineering and manufacturing."}
      (is (/ 1 1000) inch))
    (mil
      {:names ["mil" "mils"]
       :info  "A unit of length used in engineering and manufacturing."}
      (is (/ 1 1000) inch))
    (hand
      {:names ["hand" "hands"]
       :info  "A unit of length currently used for measuring the height of horses."}
      (is 0.1016 meter))

    (angstrom
      {:names ["angstrom" "angstroms"]
       :info  "An atomic-scale unit of length."}
      (is (Math/pow 10 -10) meter))
    (light-year
      {:names ["light-year" "light-years"]
       :info  "The distance traveled by light in a vacuum in one Julian year."}
      (is (* 9.4607 (Math/pow 10 15)) meter))
    (parsec
      {:names ["parsec" "parsecs"]
       :info  "PARallax of one arcSECond. A unit of distance used in astronomy."}
      (is 3.26156 light-year))
    (astronomical-unit
      {:names ["astronomical-unit" "astronomical-units"]
       :info  "The distance from the Earth to the Sun."}
      (is 149597870700 meter))

    ;; area
    (perch
      {:names ["perch" "perches"]
       :info  "An Imperial and Customary unit of area."}
      (uexp rod 2))
    (rood
      {:names ["rood" "roods"]
       :info  "An Imperial and Customary unit of area."}
      (umul furlong rod))
    (acre
      {:names ["acre" "acres"]
       :info  "An Imperial and Customary unit of area."}
      (umul furlong chain))
    (section
      {:names ["section" "sections"]
       :info  "A unit of area used in surveying."}
      (is 640 acre))
    (township
      {:names ["township" "townships"]
       :info  "A unit of area used in surveying."}
      (is 36 section))
    (are
      {:names ["are" "ares"]
       :info  "An obsolete metric unit of area."}
      (is 100 (uexp meter 2)))
    (decare
      {:names ["decare" "decares"]
       :info  "An obsolete metric unit of area."}
      (is 10 are))
    (hectare
      {:names ["hectare" "hectares"]
       :info  "A metric unit of area."}
      (is 100 are))
    (dunam
      {:names ["dunam" "dunams"]
       :info  "An Ottoman unit of area, representing the amount of land that can be plowed by one person in one day."}
      decare)
    (marabba
      {:names ["marabba" "marabbas"]
       :info  "A Pakistani unit of area."}
      (is 25 acre))
    (jareeb
      {:names ["jareeb" "jareebs"]
       :info  "A Pakistani unit of area."}
      (is (/ 1 10) acre))
    (kanee
      {:names ["kanee" "kanees"]
       :info  "A Pakistani unit of area."}
      (is 4 jareeb))
    (guntha
      {:names ["guntha" "gunthas"]
       :info  "A Pakistani unit of area."}
      (is (/ 1 4) jareeb))
    (anna
      {:names ["anna" "annas"]
       :info  "A Pakistani unit of area."}
      (is (/ 1 6) guntha))

    ;; pressure
    (bar
      {:names ["bar" "bars"]
       :info  "A non-SI unit of pressure."}
      (is 100000 pascal))
    (barye
      {:names ["barye" "baryes"]
       :info  "A non-SI unit of pressure."}
      (is (/ 1 10) pascal))
    (atmosphere
      {:names ["atmosphere" "atmosphere" "atm"]
       :info  "One standard Earth atmosphere."}
      (is 101325 pascal))
    (torr
      {:names ["torr" "torrs"]
       :info  "A non-SI unit of pressure."}
      (is (/ 1 760) atmosphere))
    (mmhg
      {:names ["mmhg" "millimeters-of-mercury" "millimetres-of-mercury"]
       :info  "A non-SI unit of pressure."}
      (is 133.322387415  pascal))
   
    ;; energy
    (erg
      {:names ["erg" "ergs"]
       :info  "A non-SI unit of energy."}
      (is (Math/pow 10 -7) joule))
    (electron-volt
      {:names ["electron-volt" "electron-volt"]
       :info  "The energy gained or lost by a charge of one electron when moved across a potential difference of one volt."}
      (is (* 1.602176565 (Math/pow 10 -19)) joule))
    (btu
      {:names ["btu" "btus" "british-thermal-unit" "british-thermal-units"]
       :info  "The amount of energy needed to heat or cool one pound of water by one degree Fahrenheit."}
      (is 1055 joule))
    (calorie
      {:names ["calorie" "calories"]
       :info  "The amount of energy needed to heat or cool one gram of water by one degree Celsius."}
      (is 4.184 joule))

    ;; force
    (pound-force
      {:names ["pound-force" "pounds-force"]
       :info  "A non-SI unit of force."}
      (is 32.174049 (umul pound (udiv foot (uexp second' 2)))))
    (dyne
      {:names ["dyne" "dynes"]
       :info  "A CGS unit of force."}
      (is (Math/pow 10 -5) newton))

    ;; power
    (horsepower
      {:names ["horsepower"]
       :info  "A non-SI unit of power."}
      (is 745.69987158 watt))

    ;; temperature
    (rankine
      {:names ["rankine" "degree-rankine" "degrees-rankine"]
       :info  "A non-SI unit of temperature. Rankine is to Fahrenheit as Kelvin is to Celsius."}
      (is (/ 5 9) kelvin))

    (fahrenheit
      {:names ["fahrenheit" "degree-fahrenheit" "degrees-fahrenheit"]
       :info  "A non-SI unit of temperature."}
      (shift -459.67 rankine))


    ;; units related to radiation and radioactivity
    (roentgen
      {:names ["roentgen" "roentgens"]
       :info  "Unit of X-ray and gamma ray kerma."}
      (is 0.000258 (udiv coulomb kilogram)))
    (rem'
      {:names ["rem" "rems"]
       :info  "Roentgen Equivalent in Mammal. A unit of effective dose of radiation."}
      (is 0.01 sievert))
    (rad
      {:names ["rad" "rads"]
       :info  "Unit of absorbed dose of radiation."}
      (is 0.01 gray))

    ;; time
    (minute
      {:names ["minute" "minutes"]
       :info "Common unit of time."}
      (is 60 second'))
    (hour
      {:names ["hour" "hours"]
       :info "Common unit of time."}
      (is 60 minute))
    (day
      {:names ["day" "days"]
       :info "Common unit of time."}
      (is 24 hour))
    (week
      {:names ["week" "weeks"]
       :info "Common unit of time."}
      (is 7 day))
    (fortnight
      {:names ["fortnight" "fortnights"]
       :info "Common unit of time (outside of North America)."}
      (is 2 week))
    (year
      {:names ["year" "years"]
       :info "Julian year, used in astronomy."}
      (is 365.25 day))
    (planck-time
      {:names ["planck-time" "planck-times"]
       :info "Time taken for light in a vacuum to travel 1 Planck length."}
      (is (* 5.39106 (Math/pow 10 -44)) second'))
    (svedberg
      {:names ["svedberg" "svedbergs"]
       :info "Unit of time used to measure sedimentation rate."}
      (is (Math/pow 10 -13) second'))

    ;; nothing else is like a sverdrup <3
    (sverdrup
      {:names ["sverdrup" "sverdrups"]
       :info  "Unit of rate of volume transport. Used in oceanography."}
      (is 1000000 (udiv (uexp meter 3) second')))))

