# hagemann-WeatherTripPlanner
R package to get weather statistics 

Task and Technique Description

Statement of the purpose: Scenario that describes the use of the Weather Trip Planner application.
Description of individual user: Anyone planning a trip anywhere on Earth.
Assumption of equipment (incl. package dependencies): A computer with R and R Shiny installed, and an internet connection.

Scenario

1.	The user opens the GitHub repository asarafoglou-ptns/hagemann-WeatherTripPlanner and opens the file shiny_app.R.

2.	The user starts the shiny app by clicking on “Run App” in the top right corner. 

3.	The web application opens and the default start page is displayed.

4.	The application displays five fields and asks the user to enter their preferences
a.	The user enters their trip destination which is any location in the format “City, Country.”
b.	User selects the season during which they plan to travel.
c.	The user selects either temperature or precipitation preferences.
i.	The user selects their temperature preferences: warm, or cold.
OR
ii.	The user selects their precipitation preferences: none, rain, snow.
d.	The user enters the number of days of the planned trip.

5.	The user clicks on “show best periods” 
a.	The application now shows five different tabs which the user can look at.
i.	The first tab, “best periods”, displays the top three periods in the indicated season matching the user’s temperature and precipitation preferences.
ii.	The second, third, and fourth tabs display each period in detail. The user can click on any tab to see the date range, the average sunlight duration (in hours), the average precipitation (in mm), the average snowfall (in mm), the average temperature (in °C), etc.
iii.	The fifth tab, “Comparison”, displays a graph for the user to compare the three periods. The user can select the weather parameters on which they want to compare the periods. 


