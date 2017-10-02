(*** hide ***)
#r @"packages\FSharp.Data\lib\net40\FSharp.Data.dll"
#load "packages/FsLab/Themes/DefaultWhite.fsx"
#load "packages/FsLab/FsLab.fsx"

(**

Analysis of Economical Indicators in South America
========================
By Gerson Noboa

In this work, a variety of economical indicators will be evaluated in South America, and correlations 
will be made with the type of government each country is having to endure.

This (hopefully) will result in interesting facts, such as the developments each country 
was facing when changing its ideologies, performance based on countries with similar regimes, and maybe
figure out if there's a tendency of certain countries with certain governments doing better than others.


First, here's a chart with the Gross Domestic Product for all of the available countries of South America.

*)

(*** hide ***)
open Deedle
open System.IO
open XPlot.GoogleCharts
open XPlot.GoogleCharts.Deedle

(**We first define all of the countries in South America, and include results for combined 
Latin America & Caribbean for reference*)

let wb = FSharp.Data.WorldBankData.GetDataContext()
let arg = wb.Countries.Argentina
let bol = wb.Countries.Bolivia
let bra = wb.Countries.Brazil
let chi = wb.Countries.Chile
let col = wb.Countries.Colombia
let ecu = wb.Countries.Ecuador
let par = wb.Countries.Paraguay
let per = wb.Countries.Peru
let uru = wb.Countries.Uruguay
let ven = wb.Countries.``Venezuela, RB``
 
(**An array with socialist countries, capitalist countris and all of them is created for easier computation of results.
Also, the names are defined to make it easier to generate graphs.*)
let allCountries = [|arg; bol; bra; chi; col; ecu; par; per; uru; ven|]
let allSocialists = [|arg; bol; bra; ecu; ven|]
let allNormal = [|chi; col; par; per; uru|]

let namesAllCountries = ["Argentina"; "Bolivia"; "Brazil"; "Chile"; "Colombia"; "Ecuador"; "Paraguay"; "Peru"; "Uruguay"; "Venezuela"]
let namesAllSocialists = ["Argentina"; "Bolivia"; "Brazil"; "Ecuador"; "Venezuela"]
let namesAllNormal = ["Chile"; "Colombia"; "Paraguay"; "Peru"; "Uruguay"]

(**
Gross Domestic Product
----------
The first figure to analyze is the Gross Domestic Product. According to Investopedia, the Gross Domestic Product is
the monetary value of all the finished goods and services produced within a country's borders in a specific time period.
Thus, the GDP can be used to analyze how the economy is doing in a specific country.

As we can see from the graph, some of these countries in recent years have either negative growth of GDP or very little variation.
Brazil's case is very dramatic. Being the country with the biggest economy of the region, its GDP has been on decline the last 
years. Also, we can see that Venezuela had some very good years under the socialist regime of Hugo Chavez. Ecuador tends to have very
little variation, even through the 2000 crisis.
*)

(*** define-output:chart1 ***)
[for c in allSocialists -> 
    c.Indicators.``GDP growth (annual %)``]
    |> Chart.Line
    |> Chart.WithLabels namesAllSocialists
(*** include-it:chart1 ***)

(**
If we compare it to capitalist countries, we see that only Paraguay has had GDP drops in the last years. The other countries
have a stable increase every year. It is curious to see Paraguay's harsh variations from one year to another. Also, growth
seems to be declining for all of them.
*)
(*** define-output:chart2 ***)
[for c in allNormal -> 
    c.Indicators.``GDP growth (annual %)``]
    |> Chart.Line
    |> Chart.WithLabels namesAllNormal
(*** include-it:chart2 ***)

(**
Industry, value added
----------
Next is the Industry indicator. According to the World Data Bank, the Industry, value added indicator is the 
net output of a sector after adding up all outputs and subtracting intermediate inputs. This means that the output of
manufacturing, construction and similar are all rounded up under this indicator. Even though I thought that socialist
countries would have a lower score on this indicator, it turns out that the biggest performer here is Venezuela,
a country that is immersed in a huge crisis.
*)

(*** define-output:chart3 ***)
[for c in allCountries -> 
    c.Indicators.``Industry, value added (% of GDP)``]
    |> Chart.Line
    |> Chart.WithLabels namesAllCountries
(*** include-it:chart3 ***)

(**
Debt
----------
One of the biggest characteristics of socialist countries is the huge amount of debt they accumulate in order
to execute its projects. These debts tend to be long term instead of short term. We can see a tendency here in which
debt starts increasing after socialist governments are established. This is especially true for Brazil,
however, the debt starts its rise in debt with the government of Lula (which isn't a socialist), and continues to grow
a lot during Dilma Rouseff's period.

Argentina is a weird case, since Kirchner started its period in 2003, but debt started to increase before that. We can
also see Venezuela's rise in debt starting from 2007, which is the year in which oil prices started to soar. Being
one of the biggest oil exporters in the world, this could have been a good situation to reduce debt, not increase it.
*)
(*** define-output:chart4 ***)
[for c in allSocialists -> 
    c.Indicators.``External debt stocks, long-term (DOD, current US$)``]
    |> Chart.Line
    |> Chart.WithLabels namesAllSocialists
(*** include-it:chart4 ***)

(**
As said previously, socialist countries tend to accumulate long-term debt, not short. Even with that fact,
we can see Argentina and Venezuela accumulating short-term debt as well. Probably, it is not a coincidence
that both of these countries have the weakest economies of the continent right now.
*)
(*** define-output:chart5 ***)
[for c in allSocialists -> 
    c.Indicators.``Short-term debt (% of total reserves)``]
    |> Chart.Line
    |> Chart.WithLabels namesAllSocialists
(*** include-it:chart5 ***)

(*** define-output:chart6 ***)
[for c in allSocialists -> 
    c.Indicators.``Short-term debt (% of total external debt)``]
    |> Chart.Line
    |> Chart.WithLabels namesAllSocialists
(*** include-it:chart6 ***)

(**
Unfortunately, there are very few data points for this indicator, but it is good enough to make a small analysis.
We can see that Brazil's debt is just unbelievably high, and the effects of socialism tendencies will take years
upon years for the country to recover from. Also, even though Bolivia is a bigger country than Ecuador, the latter still
managed to put itself in greater debt than the former, something that, as an Ecuadorian, makes me really angry...
*)
(*** define-output:chart7 ***)
[for c in allSocialists -> 
    c.Indicators.``Present value of external debt (current US$)``]
    |> Chart.Line
    |> Chart.WithLabels namesAllSocialists
(*** include-it:chart7 ***)

(**
Imports and Exports
----------
If there's something to point out here, is the diminishing amount of exports being done by all countries.
Venezuela and Ecuador's decline starts just as oil prices start to drop, which explains why both exports and imports
are reduced, since there has been less money in these countries to produce and export stuff, and to import products.
Another case for imports decrease, at least in Ecuador and Venezuela, is heavy taxation on imported goods.
This is especially a problem for Ecuador, since we use US$ as a currency, so importing goods means taking money out of
the country, something which the government is trying to avoid through taxation.
*)

(*** define-output:chart8 ***)
[for c in allSocialists -> 
    c.Indicators.``Imports of goods and services (annual % growth)``]
    |> Chart.Line
    |> Chart.WithLabels namesAllSocialists
(*** include-it:chart8 ***)

(*** define-output:chart9 ***)
[for c in allSocialists -> 
    c.Indicators.``Exports of goods and services (% of GDP)``]
    |> Chart.Line
    |> Chart.WithLabels namesAllSocialists
(*** include-it:chart9 ***)

(**
Taxes
----------
Another charateristic of socialist countries is heavy taxation. Argentina's case is quite dramatic. We
can also see the rise of taxation through Dilma Rouseff's period. Something astonishing for me is seeing
Ecuador's figures on decline, since throughout socialist Rafael Correa's presidency (from 2006 until 2017)
taxes have increased in almost every sector.
*)
(*** define-output:chart10 ***)
[for c in allSocialists -> 
    c.Indicators.``Total tax rate (% of commercial profits)``]
    |> Chart.Line
    |> Chart.WithLabels namesAllSocialists
(*** include-it:chart10 ***)

(**
In this graph we can compare the taxation on socialist countries vs capitalist countries. There's a clearly
marked tendency to have lower taxation on capitalist countries, which Chile, the strongest economy in South America,
having the lowest tax rate.
*)

(*** define-output:chart11 ***)
[for c in allCountries -> 
    c.Indicators.``Total tax rate (% of commercial profits)``]
    |> Chart.Line
    |> Chart.WithLabels namesAllCountries
(*** include-it:chart11 ***)

(**
Inflation
----------
Inflation is another thing that socialism increases, especially on countries that are hit by crisis through the
socialism regime (something which has happened in all socialist regimes in the region). However, we can see here that
each country has had worse times than this one. This is especially true for Bolivia, which had a mind-blowing 11749.64 inflation
in 1985. Thus, this graph is very difficult to analyze because scales are messed up. A graph has been generated
for the remaining four countries.
*)

(*** define-output:chart13 ***)
[for c in allSocialists -> 
    c.Indicators.``Inflation, consumer prices (annual %)``]
    |> Chart.Line
    |> Chart.WithLabels namesAllSocialists
(*** include-it:chart13 ***)

(**
Thanks to dollarization in 2000, Ecuador reduced its nonsensical inflation values. However, in recent years
we haven't been able to replicate the very low inflation values from 2004-2007.
*)
(*** define-output:chart14 ***)
ecu.Indicators.``Inflation, consumer prices (annual %)``
    |> Chart.Line
    |> Chart.WithLabel "Ecuador"
(*** include-it:chart14 ***)

(**
Since the oil bonanza stopped, Venezuela hasn't been able to recover, and in 2015 it reached a mind-boggling 121% inflation value.
Probably in 2016 it didn't fared any better, and the 100 Bs. bill had to be removed from the market because it was so 
worthless that making a photocopy of it was more expensive than the bill itself (130 Bs.).
*)
(*** define-output:chart15 ***)
ven.Indicators.``Inflation, consumer prices (annual %)``
    |> Chart.Line
    |> Chart.WithLabel "Venezuela"
(*** include-it:chart15 ***)

(**
Argentina's inflation is a serious problem, since year by year it increases at least by 6%. We can see in 2002, just before Kirchner's
rise to power, the inflation reached a peak for the millenium of 25%. However, the worst year for Argentina was undoubtedly 1989, in which
inflation reached a staggering 3079%. Fortunately for them, those days are over.
*)
(*** define-output:chart16 ***)
arg.Indicators.``Inflation, consumer prices (annual %)``
    |> Chart.Line
    |> Chart.WithLabel "Argentina"
(*** include-it:chart16 ***)

(**
Inflation for Brazil is steady throughout the recent years, and we can see that in 1990 it reached a peak of almost 3000%.
It seems like socialism hasn't affected inflation very much in the case of inflation in Brazil.
*)

(*** define-output:chart17 ***)
bra.Indicators.``Inflation, consumer prices (annual %)``
    |> Chart.Line
    |> Chart.WithLabel "Brazil"
(*** include-it:chart17 ***)

(**
On a side note, we can see that all of these countries have a spike in inflation values in 1989. Researching a bit took me to
a series of events that are collectively called "La decada perdida" (The lost decade, accents removed because of problems in generation), 
in which Latin American countries could not repay their debts and had to default on them on the 1980s This stopped investments
and loans to the region and made its population very vulnerable. The whole region entered into a crisis in which unemployment 
soared and prices increased dramatically. Perhaps, the reason for all of these spikes in inflation values could be attributed 
to the lasting effects of La decada perdida.
*)

(**
Conclusion
----------
We can see that countries under socialist regimes all behave the same way most of the time: inflation, foreign debt and taxation.
This is specially evident in the graphs where indicators have been compared to capitalist countries. However, we can also
see that the region is marred with mismanagement and errors throughout the years. These problems have created issues on
different years and have increased some indicators much more than what socialism has done in recent years.

Unfortunately for the region, governments are involved in scandals and bad decisions regardless of the political line of thought
they have. Nevertheless, it is interesting to see how in many areas, socialist countries present the exact same problems in the same 
period of time throughout different parts of the continent, across countries whose main activities and sources of income differ
from each other. Socialism is either a failed model or a model that is hard to implement in the region, thanks to corruption,
size of economies and main activities of each country.

I also expected some factors to be more vastly different among countries with socialism implemented vs capitalism. This is 
especially true for taxation. I just find it hard to believe that we have the same rate of taxation than countries like Peru and
Chile, where taxes are much lower and on a less amount of industries. Even though tendencies were able to be drawn, I thought
that they would be more prominent.

References
----------
* http://www.investopedia.com/terms/g/gdp.asp
* http://interactioncouncil.org/node/74
* http://www.angelfire.com/nj/GregoryRuggiero/latinamericancrisis.html
* https://en.wikipedia.org/wiki/Latin_American_debt_crisis
* https://www.google.lt/search?hl=en&q=venezuelan%20presidents&ei=rPVkWP6dJcWvswHA1ZXABg
* https://www.google.lt/search?hl=en&q=argentinian%20presidents&ei=Z_VkWPH8OcaVsgGE_ajAAg
* http://data.worldbank.org/indicator/NV.IND.TOTL.ZS?view=chart
* https://www.google.lt/search?hl=en&q=brazilian%20presidents&ei=__FkWKSLGYKyswHbhrGQDg#hl=en&q=Luiz+In%C3%A1cio+Lula+da+Silva&stick=H4sIAAAAAAAAAONgVuLQz9U3KEi2LHzEmM0t8PLHPWGppElrTl5jjOESCEpNTs0rCShKLc5MATKKhTy4OHzykxNLMvPzhKS4eKTAmg1N04o0GKS4uOA8KQUlLl6Nl83fRQ9c4XmoJcR5XuPM6-ORcxgEk-00P7GJRmnyAABEFjb6egAAAA
*)