# Geautomatiseerd testen met Erlang/OTP

Erlang/OTP is ontworpen voor het bouwen van grote, schaalbare, soft-realtime systemen met een hoge beschikbaarheid. Het testen van dergelijke systemen is niet eenvoudig, laat staan [[geautomatiseerd testen]]. Voor Erlang zijn er dan ook geavanceerde automatische test methoden beschikbaar.

De drie belangrijkste methoden worden hier kort besproken aan de hand van een test project. Het project staat op github.

git clone git@github.com:wardbekker/ci_quickstart.git

## Unit testing met EUnit

We beginnen bij de eenvoudigste; EUnit. Dit is een unit testing bibliotheek voor Erlang. In een unit test controleer je of de functie goed werkt bij bekende input en resultaat. 

<script src="https://gist.github.com/3690396.js?file=ci_quickstart_math.erl"></script>

Het slechte nieuws is dat de waarde van deze test zeer laag is. Weten we nu zeker dat optelling goed gaat in alle gevallen? Het enige wat de test nu aantoond is dat het goed gaat voor deze inputs en niet goed gaat bij deze inputs. Stel, ik herschrijf de functie. Uiteraard compleet verkeerd, maar de testen blijven gewoon 'green'. Stel, de argumenten zijn [[Small integers]], en die hebben een bereik van -576460752303423489 tot 576460752303423488. Met twee argumenten, betekend dit dat er enorm veel verschillende input mogelijk is. En in de unit test doen we er maar 3!?!?  Ook al ben je een harde werker en test je wel 10! addities, in feite is de waarde van de unit test niet verbeterd en nog steeds erg laag. 

## QuickCheck

Er is een oplossing voor de tekortkoming van unit testen. Wat je eigenlijk wilt is een test methode dat alle mogelijke input variaties kan genereren en de bijbehorende output kan controleren. Deze methode heet QuickCheck. Voor Erlang zijn er een aantal uitstekende frameworks beschikbaar Triq, Quvic QuickCheck en ProPEr. 

<<Hieronder een voorbeeld van de dezelfde test als een zgn QuickCheck eigenschap / property.>>

Specifieke nummers worden niet getest. We gaan nu controleren of de functie voldoet aan de eigenschap dat als je X weer erafhaalt, je Y overhoud. 

Bij elke test generereerd Quickcheck random integers voor elk argument. Omwille van een korte test tijd tijdens development zijn dit er standaard 100, en dit aantal is voor uitgebreidere testen stevig op te voeren.

De uitdaging bij het werken met QuickCheck is het bedenken van de eigenschappen van de functie. Dit is lastiger dan het maken van een unit test. Sterker nog, het schrijven van de functie is vaak nog eenvoudiger dan het redeneren over de eigenschappen. Het positieve effect van QuickCheck op de kwaliteit van je code, en de manier waarop je als developer over je code nadenkt maakt deze tool een zeer waardevol onderdeel van je test gereedschapskist.

[[Naast testen van functie eigenschappen is Quickcheck erg goed in het testen van zgn .State Machine. Een goed voorbeeld hiervan is de controle van de beloofde eventual concisteit van Basho's Riak, een populair distribueerd database systeem gemaakt in Erlang. Zie hiervoor de slides van xxxxx. ]]

## Common test 

Zoals bekend is Erlang uitermate geschikt voor het bouwen van concurrent, distrubuted en fault tolerant systemen. Om te controleren of je systeem werkt zoals beloofd, is heel lastig.

Hiervoor is Common Test in het leven geroepen. Dit krachtige test framework is uitermate geschikt voor de ontwikkeling van pittige systeem tests. De inherente complexiteit van concurrent, distrubuted en fault tolerant systemen maakt ook Common Test complex. Hoe je een serieuze OTP applicatie op de pijnbank legt met CT valt derhalve buiten de scope van deze blogpost. 

<<Een eenvoudige vertaling van onze EUNIT test naar CT ziet er als volgt uit>>

## Continious integration

<<starten met definitie>>

Stel, je hebt een flinke hoeveelheid automatische testen geimplementeerd. Het draaien van alle geavanceerde testen kan hierdoor lang duren en je ontwikkel systeem flink belasten Om deze, en [[nog meer goede redenen]], is het aan te raden om gebruik te maken van een continious integration omgeving. 

Er zijn legio systemen waarmee het mogelijk is om dit voor Erlang op te zetten. In deze post gebruiken we het hosted systeem Travis-CI. Deze dienst ondersteunt Erlang, integreert met het Populaire github en zorgt voor een vliegende start. En het is gratis voor open source projecten. 

In dit filmje wordt uitgelegd hoe je je erlang project kan laten testen door Travis-CI.

Als de intstructies goed zijn opgevolgd krijg je de volgende mail binnen als alle testen slagen.

Elke keer als je nu nieuwe code naar github 'pushed', worden de autmatische testen gedraaid voor de x laatste versies van Erlang.

## Samenvatting.








