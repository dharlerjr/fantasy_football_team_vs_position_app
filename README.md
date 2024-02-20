# Fantasy Football: Team vs. Position Shiny App

[Shiny App Link](https://david-harler-jr.shinyapps.io/team-vs-pos-app/)

## Purpose

In this analysis, I created a Shiny application that allows users to see how each NFL team performed against offensive skill position groups during the 2023 season, in terms of fantasy points-per-reception (PPR) points allowed. First, the app displays a static table which provides a league overview for the entire fantasy footall season. As you scroll down however, the app now allows the user to select one team and one position at a time, allowing for a more in-depth analysis. For example, by default, the app is set to display the Arizona Cardinals' performance against opposing quarterbacks from weeks 1 to 17. A line chart is then displayed to visualize how many PPR points quarterbacks earned against the Cardinals from week to week. Additionally, a box plot and a table of summary statistics are also provided, informing the user of important characteristics of the Cardinals' performance, such as how many points they allowed to quarterbacks on average. Lastly, a data table of the scores is also displayed for the user's convenience. Each of these four components - the line chart, box plot, table of summary statistics and data table - are reactive, meaning as the user changes updates their input, the components will be also updated. For instance, the user may want to see how the Detroit Lions performed against running backs after their bye week, or how the Kansas City Chiefs performed against wide receivers during the last four weeks of the season. The App allows the user to easily perform each of these analyses.

## Results

Here is a the static league overview table, displayed at the top of the app.

![League Overview Left](https://github.com/dharlerjr/fantasy_football_team_vs_position_app/blob/main/Images/LeagueOverviewLeft.png)
![League Overview Right](https://github.com/dharlerjr/fantasy_football_team_vs_position_app/blob/main/Images/LeagueOverviewRight.png)

Here is a screenshot of the app upon startup.

![Startup](https://github.com/dharlerjr/PyBer_Analysis/blob/main/analysis/Fig8_City_Type_Summary.png)

## Sample Insights

1. The Carolina Panthers allowed the fewest points to opposing quarterbacks throughout the fantasy football season
2. The Washington Commanders performed poorly against all skill position players.
3. In week 3, the Denver Broncos gave up 54.6 PPR points to Miami running backs.

## Improvements

- Graphical interface: While the app is functional, I'm sure layout of the app's components could improve. For example, maybe the static table could be moved to its own page, or maybe the navigation bar headings could be bolded. While I'm sure there are many other minor improvements that a graphic or UI designed could make, I was happy with the final layout.
- Occasionally, the data table will display scores with more than 8 decimal places. I'm not sure why this occurs, but luckily, I haven't encountered this error very often, so I've left it as is.

## More Notes

- The data I used was retrieved from FantasyPros.com. However, I'm sure plenty of resources exist online for retrieving fantasy football data.
- Team logos and wordmarks are courtesy of the nflfastR team. These logos and wordmarks are by far my favorite feature, and I'm glad I able to figure out how to render them on my app.
