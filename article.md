# Geautomatiseerd testen met Erlang/OTP

Erlang/OTP is ontworpen voor het bouwen van grote, schaalbare, soft-realtime systemen met een hoge beschikbaarheid. Het testen van dergelijke systemen is niet eenvoudig, laat staan [geautomatiseerd testen](http://en.wikipedia.org/wiki/Software_testing#Automated_testing). Voor Erlang zijn er dan ook geavanceerde automatische test methoden beschikbaar.

De drie belangrijkste methoden worden hier kort besproken aan de hand van een test project. De methoden zijn:

* [Unit testing](#unit-testing-met-eunit)
* [Quickcheck](#quickcheck)
* [Common test](#common-test)

Je kan het test project *clonen* van Github met het volgende commando: 

```sh
$ git clone git@github.com:wardbekker/ci_quickstart.git
```

Voor het compileren van het project en uitvoeren van de testen wordt [Rebar](https://github.com/basho/rebar) gebruikt, een *sophisticated build-tool for Erlang projects that follows OTP principles*.  Je bouwt Rebar als volgt:

```sh
$ git clone git://github.com/basho/rebar.git
$ cd rebar
$ ./bootstrap
Recompile: src/getopt
...
Recompile: src/rebar_utils
==> rebar (compile)
Congratulations! You now have a self-contained script called "rebar" in
your current working directory. Place this script anywhere in your path
and you can use rebar to build OTP-compliant apps.
```



## Unit testing met EUnit ##

Je begint met de eenvoudigste test methode; [EUnit](http://www.erlang.org/doc/apps/eunit/chapter.html). Dit is een unit testing bibliotheek voor Erlang. In een unit test controleer je of de functie goed werkt bij bekende input en resultaat. In dit voorbeeld heb je de functie `addition` geimplementeerd in de module `ci_quickstart_math` en twee *assertions*. (Je voert deze test uit op de commandline met: `rebar get-deps compile eunit`):

```erlang
-module(ci_quickstart_math).
-export([addition/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

addition(X, Y) ->
    X + Y.

-ifdef(TEST).

simple_test() ->
    ?assertEqual(4, addition(2,2)),
    ?assertNotEqual(3, addition(1,1)).

-endif.
```

Het slechte nieuws is dat de waarde van deze test zeer laag is. Weet je nu zeker dat optelling goed gaat in alle gevallen? Het enige wat de test nu aantoont is dat:

* `addition(2,2) == 4`
* `addition(1,1) /= 3`

Stel, je verandert de implementatie van de function `addition` in:

```erlang
 addition(X, Y) ->
    4.
```

De testen slagen in dit geval, maar dit betekend niet dat de implementatie van `addition` correct is.

Sterker nog; De argumenten zijn in dit geval [64-bit small integers](http://www.erlang.org/doc/efficiency_guide/advanced.html), en die hebben een bereik van -576460752303423489 t/m 576460752303423488. Met twee argumenten, betekend dit dat er enorm veel verschillende input mogelijk is. En in de unit test controleer je er maar 3!?!?  Ook al ben je een harde werker en test je wel 10! addities, in feite is de waarde van de unit test niet verbeterd en nog steeds erg laag. 

Wat nu?

## QuickCheck ##

Wat je eigenlijk wil is een test methode dat alle mogelijke input variaties kan genereren en de bijbehorende output kan controleren. Deze methode heet [QuickCheck](http://en.wikipedia.org/wiki/QuickCheck). Voor Erlang zijn er een aantal QuickCheck frameworks beschikbaar:

*  [Quvic QuickCheck](http://www.quviq.com)
*  [ProPEr](https://github.com/manopapad/proper)
* [Triq](https://github.com/krestenkrab/triq)

Een Quickcheck test voor de `addition` functie:

```erlang
prop_sum() ->
    ?FORALL(
        {X, Y}, 
        {int(), int()},
        addition(X,Y) - Y == X
    ).
```

Specifieke nummers worden niet getest. Je gaat nu controleren of de functie voldoet aan de eigenschap dat als je Y weer er afhaalt, je X overhoud.

Bij elke test genereert Quickcheck random integers voor elk argument. Standaard worden er 100 combinaties getest, en dit aantal voer je op .

De uitdaging bij het werken met QuickCheck is het bedenken van de eigenschappen van de functie. Dit is lastiger dan het maken van een unit test. Sterker nog, het schrijven van de functie is vaak nog eenvoudiger dan het redeneren over de eigenschappen. Het positieve effect van QuickCheck op de kwaliteit van je code, en de manier waarop je als developer over je code nadenkt maakt deze tool een zeer waardevol onderdeel van je test gereedschapskist.

[[Naast testen van functie eigenschappen is Quickcheck erg goed in het testen van zgn .State Machine. Een goed voorbeeld hiervan is de controle van de beloofde eventual consistency van Basho's Riak, een populair distribueerd database systeem gemaakt in Erlang. Zie hiervoor de slides van xxxxx. ]]

## Common test ##

Zoals bekend is Erlang uitermate geschikt voor het bouwen van concurrent, distributed en fault tolerant systemen. Om te controleren of je systeem werkt zoals verwacht, is complex.

Hiervoor is [Common Test](http://www.erlang.org/doc/apps/common_test/basics_chapter.html) in het leven geroepen. Dit krachtige test framework is uitermate geschikt voor de ontwikkeling van pittige [systeem tests](http://en.wikipedia.org/wiki/System_testing). De inherente complexiteit van concurrent, distributed en fault tolerant systemen maakt ook Common Test complex. Hoe je een serieuze OTP applicatie op de pijnbank legt met CT valt derhalve buiten de scope van deze blogpost. 

```erlang
-module(basic_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([test1/1, test2/1]).

all() -> [test1,test2].

test1(_Config) ->
    3 = ci_quickstart_math:addition(1,2). %% validated using pattern matching

test2(_Config) ->
    2 = ci_quickstart_math:addition(1,1).  %% validated using pattern matching
```

## Continuous integration met Travis-CI

Stel, je hebt een flinke hoeveelheid automatische testen geïmplementeerd. Het uitvoeren van alle geavanceerde testen kan hierdoor lang duren en je ontwikkel systeem flink belasten Om deze, en [nog meer goede redenen](http://en.wikipedia.org/wiki/Continuous_integration#Advantages_and_disadvantages), is [Continuous integration](http://en.wikipedia.org/wiki/Continuous_integration) aan te raden. 

Er zijn legio systemen waarmee het mogelijk is om dit voor Erlang op te zetten. In deze post wordt het hosted systeem [Travis-CI](http://travis-ci.org) gebruikt als voorbeeld. Deze dienst ondersteunt Erlang, integreert met het populaire Github en zorgt voor een vliegende start. En het is gratis voor open source projecten. 


### Voorbereiding

Het build proces van Travis-CI configureer je via het `.travis.yml`-bestand in de *root* van je repository. Een voorbeeld:

```yaml
language: erlang // De repository bevat een Erlang project
notifications: 
  email: you@example.org // Build success en failures stuurt Travis-CI naar dit adres.
otp_release: // Travis-CI test/bouwt je project voor meerdere Erlang/OTP versies.
  - R15B01
  - R15B
  - R14B04
```

### Travis-CI Setup

Deze video toont hoe je start met Travis-CI:

* Log in met je Github account.
* Ga naar de Travis-CI *profile* pagina.
* Schakel de gewenste Github *repository* in.

That's it!

<a href="http://www.youtube.com/watch?v=YxJJu6mShiA&hd=1" target="_blank">![Setup](https://raw.github.com/wardbekker/ci_quickstart/master/images/signing_and_switch.png)</a>

### Travis-CI Success Run

Deze video toont hoe Travis-CI een geslaagde *integration build*
rapporteerd:

<a href="http://www.youtube.com/watch?v=rJWRY1Uf_qg&hd=1" target="_blank">![Success](https://raw.github.com/wardbekker/ci_quickstart/master/images/success.png)</a>

### Travis-CI Failure Run

Deze video toont hoe Travis-CI een mislukte *integration build*
rapporteerd:

<a href="http://www.youtube.com/watch?v=5u8Kpz3m8ho&hd=1" target="_blank">![Fail](https://raw.github.com/wardbekker/ci_quickstart/master/images/fail.png)</a>

Als je e-mail adres in `.travis.yml` staat, krijg je ook een e-mail notificatie dat de laatste *commit* de build gebroken heeft:

<img src="https://raw.github.com/wardbekker/ci_quickstart/master/images/broken_email.png" width="400" height="200" alt="Broken build e-mail notification" />

Als de fout verholpen is, krijg je de volgende e-mail als de build weer slaagt:

<img src="https://raw.github.com/wardbekker/ci_quickstart/master/images/fixed_email.png " width="400" height="200" alt="Fixed build e-mail notification" />

## Samenvatting.

TODO
