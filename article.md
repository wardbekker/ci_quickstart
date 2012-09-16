# Geautomatiseerd testen met Erlang/OTP

Erlang/OTP is ontworpen voor het bouwen van grote, schaalbare, soft-realtime systemen met een hoge beschikbaarheid. Het testen van dergelijke systemen is niet eenvoudig, laat staan [geautomatiseerd testen](http://en.wikipedia.org/wiki/Software_testing#Automated_testing). Voor Erlang zijn er dan ook geavanceerde automatische test methoden beschikbaar.

De drie belangrijkste methoden worden hier kort besproken aan de hand van een test project. De methoden zijn:

* [Unit testing](#unit-testing-met-eunit)
* [Quickcheck](#quickcheck)
* [Common test](#common-test)

Je kan het test project *clonen* van Github met het volgende commando: 

```bash
git clone git@github.com:wardbekker/ci_quickstart.git
```

## Unit testing met EUnit ##

We beginnen bij de eenvoudigste; [EUnit](http://www.erlang.org/doc/apps/eunit/chapter.html). Dit is een unit testing bibliotheek voor Erlang. In een unit test controleer je of de functie goed werkt bij bekende input en resultaat. 

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

Het slechte nieuws is dat de waarde van deze test zeer laag is. Weten we nu zeker dat optelling goed gaat in alle gevallen? Het enige wat de test nu aantoont is dat:

* `addition(2,2) == 4`
* `addition(1,1) /= 3`

Stel, we veranderen de function `addition`:


```erlang
 addition(X, Y) ->
    4.
```

De testen slagen in dit geval, maar dit betekend niet dat de implementatie van `addition` correct is.

Sterker nog; De argumenten zijn in dit geval [64-bit small integers](http://www.erlang.org/doc/efficiency_guide/advanced.html), en die hebben een bereik van -576460752303423489 t/m 576460752303423488. Met twee argumenten, betekend dit dat er enorm veel verschillende input mogelijk is. En in de unit test doen we er maar 3!?!?  Ook al ben je een harde werker en test je wel 10! addities, in feite is de waarde van de unit test niet verbeterd en nog steeds erg laag. 

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

Specifieke nummers worden niet getest. We gaan nu controleren of de functie voldoet aan de eigenschap dat als je Y weer er afhaalt, je X overhoud.

Bij elke test genereert Quickcheck random integers voor elk argument. Standaard worden er 100 combinaties getest, en dit aantal is voor uitgebreidere testen stevig op te voeren.

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

Stel, je hebt een flinke hoeveelheid automatische testen geïmplementeerd. Het draaien van alle geavanceerde testen kan hierdoor lang duren en je ontwikkel systeem flink belasten Om deze, en [nog meer goede redenen](http://en.wikipedia.org/wiki/Continuous_integration#Advantages_and_disadvantages), is [Continuous integration](http://en.wikipedia.org/wiki/Continuous_integration) aan te raden. 

Er zijn legio systemen waarmee het mogelijk is om dit voor Erlang op te zetten. In deze post gebruiken we het hosted systeem [Travis-CI](http://travis-ci.org). Deze dienst ondersteunt Erlang, integreert met het Populaire github en zorgt voor een vliegende start. En het is gratis voor open source projecten. 

### Travis-CI Setup

<a href="http://www.youtube.com/watch?v=YxJJu6mShiA">![Setup](https://raw.github.com/wardbekker/ci_quickstart/master/images/signing_and_switch.png)</a>

### Travis-CI Success Run

<a href="http://www.youtube.com/watch?v=rJWRY1Uf_qg">![Setup](https://raw.github.com/wardbekker/ci_quickstart/master/images/success.png)</a>


### Travis-CI Failure Run

<a href="http://www.youtube.com/watch?v=5u8Kpz3m8ho">![Setup](https://raw.github.com/wardbekker/ci_quickstart/master/images/failure.png)</a>

![Broken Build E-mail notification](https://raw.github.com/wardbekker/ci_quickstart/master/images/broken_email.png)

![Fixed Build E-mail notification](https://raw.github.com/wardbekker/ci_quickstart/master/images/fixed_email.png)

In dit filmje wordt uitgelegd hoe je je erlang project kan laten testen door Travis-CI.

Als de instructies goed zijn opgevolgd krijg je de volgende mail binnen als alle testen slagen.

Elke keer als je nu nieuwe code naar github 'pushed', worden de automatische testen gedraaid voor de x laatste versies van Erlang.

## Samenvatting.

TODO
