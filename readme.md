This package has tutorials for learning R as part of POL306 at Miami University. Below are steps to install it. 

Steps:
------
0. [Have R and RStudio installed.](https://kevinreuning.com/resources/install_r/)
1. Open RStudio. On the leftside there will be a box called "R Console" you will type everything I tell you to in there.
2. In the console type the below line and hit enter. This will install a package called `remotes`
~~~~ R
install.packages("remotes")
~~~~
3. Some text should popup that ends with 'The downloaded binary packages are in'  followed by a long file path. If there is any red text you might have an error, contact me.
4. Now type the below line and hit enter to install `POL306` which has all the tutorials in it
~~~~ R
remotes::install_github("reuning/POL306")
~~~~
5. You will see more text again, although this time there will be a lot more. Generally, as long as there is no red text it should be fine. It might ask your permission to install things from source, if so tell it yes. 
6. Run one last line to check to make sure everything worked. You should see a message welcoming you to the tutorial, if you see "Error in library(POL306) : there is no package called ‘kevin’" then contact me.
~~~~ R
library(POL306)
~~~~
7. Close and reopen RStudio, you should have a Tutorial tab in the top right, click on it. It should list *Introduction to R* as the first tutorial. If not, contact me.


What are we doing here?
------

I wrote our tutorials into an R package, and have it available for anyone on [Github](https://github.com/reuning/pol306). Github is used to share code and programs (as well as to develop them with a community). You can install a package from Github, but not natively in R. Instead we need the `remotes` package, which has a function called `install_github()`. So what we do is install remotes (`install.packages('remotes')`) then access the `install_github()` function from it to install my package (`remotes::install_github('reuning/POL306')`). Finally we load the `POL306` package by calling `library(POl306)`. We do not need to do this, but I wrote it so that if it loads successfully you will get a nice little message. 
