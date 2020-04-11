data("airquality")
str(airquality)

#head(data,n) and tail(data,n)

head(airquality, 5)
tail(airquality, 5)
summary(airquality)

plot(airquality$Ozone)

plot(airquality$Ozone, airquality$Wind)

plot(airquality)

# points and lines 
plot(airquality$Ozone, type= "b")

# high density vertical lines.
plot(airquality$Ozone, type= "h")


plot(airquality$Ozone, xlab = 'ozone Concentration', ylab = 'No of Instances', main = 'Ozone levels in NY city', col = 'green' ,type = 'h')

# Horizontal bar plot
barplot(airquality$Ozone, main = 'Ozone Concenteration in air',xlab = 'ozone levels', col= 'green',horiz = TRUE)

hist(airquality$Solar.R)

hist(airquality$Solar.R, main = 'Solar Radiation values in air',xlab = 'Solar rad.', col='#448585')

#Single box plot
boxplot(airquality$Solar.R)

boxplot(airquality[,0:4], main='Multiple Box plots')

par(mfrow=c(3,3), mar=c(2,5,2,1), las=1, bty="n")
plot(airquality$Ozone)
plot(airquality$Ozone, airquality$Wind)
plot(airquality$Ozone, type= "c")
plot(airquality$Ozone, type= "s")
plot(airquality$Ozone, type= "h")
barplot(airquality$Ozone, main = 'Ozone Concenteration in air',xlab = 'ozone levels', col='green',horiz = TRUE)
hist(airquality$Solar.R)
boxplot(airquality$Solar.R)
boxplot(airquality[,0:4], main='Multiple Box plots')

library(lattice)  

gear_factor<-factor(gear,levels=c(3,4,5),
                    labels=c("3gears","4gears","5gears")) 

cyl_factor <-factor(cyl,levels=c(4,6,8),
                    labels=c("4cyl","6cyl","8cyl"))





densityplot(~mpg, main="Density Plot",  xlab="Miles per Gallon")


# Installing & Loading the package
install.packages("lattice")
library(lattice)  
#Loading the dataset
attach(mtcars)
require(tigerstats)

# Exploring the dataset
head(mtcars)


library(lattice)  

gear_factor<-factor(gear,levels=c(3,4,5),
                    labels=c("3gears","4gears","5gears")) 

cyl_factor <-factor(cyl,levels=c(4,6,8),
                    labels=c("4cyl","6cyl","8cyl"))





densityplot(~mpg, main="Density Plot",  xlab="Miles per Gallon")


splom(mtcars[c(1,3,4,5,6)], main="MTCARS Data")



xyplot(mpg~wt|cyl_factor*gear_factor,  
       main="Scatterplots : Cylinders and Gears",  
       ylab="Miles/Gallon", xlab="Weight of Car")


#*************************************************************************************

#Installing & Loading the package 

install.packages("ggplot2") 
library(ggplot2)

#Loading the dataset
attach(mtcars)
# create factors with value labels 

mtcars$gear <- factor(mtcars$gear,levels=c(3,4,5),  
                      labels=c("3gears", "4gears", "5gears"))  
mtcars$am <- factor(mtcars$am,levels=c(0,1),  
                    labels=c("Automatic","Manual"))  
mtcars$cyl <- factor(mtcars$cyl,levels=c(4,6,8),  
                     labels=c("4cyl","6cyl","8cyl"))

ggplot(data = mtcars, mapping = aes(x = wt, y = mpg)) + geom_point()


ggplot(data = mtcars, mapping = aes(x = wt, y = mpg, color = as.factor(cyl))) + geom_point()


ggplot(data = mtcars, mapping = aes(x = wt, y = mpg, size = qsec)) + geom_point()

p  <-  ggplot(mtcars,aes(mpg, wt, shape  =  factor(cyl)))
p + geom_point(aes(colour  =  factor(cyl)), size  =  4) + geom_point(colour  =  "grey90", size  =  1.5)

#**************************************plotly*******************************
#Installing & Loading the package 

install.packages("plotly")  
library(plotly)


p <- plot_ly(data = mtcars, x = ~hp, y = ~wt)
p


p <- plot_ly(data = mtcars, x = ~hp, y = ~wt, marker = list(size = 10, color = 'rgba(255, 182, 193, .9)', line = list(color = 'rgba(152, 0, 0, .8)', width = 2)))
p


#***********************************************************************
data1 <- rnorm(100, mean = 10)   
data2 <- rnorm(100, mean = 0)   
data3 <- rnorm(100, mean = -10)   
x <- c(1:100)
data <- data.frame(x, data1, data2, data3)
p <- plot_ly(data, x = ~x)%>%   
  
add_trace(y = ~data1, name = 'data1',mode = 'lines')%>%             
add_trace(y = ~data2, name = 'data2', mode = 'lines+markers')%>% 
add_trace(y = ~data3, name = 'data3', mode = 'markers')
p
#************************************************************************

p <- plot_ly(data = mtcars, x =~hp, y = ~wt,color = ~hp, size = ~hp )
p

#*************************************************************************
install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
                   "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))

library("ggplot2")
theme_set(theme_bw())
library("sf")

library("rnaturalearth")
library("rnaturalearthdata")

install.packages("rgeos")
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

ggplot(data = world) +
  geom_sf()

#*****************************************************************************

ggplot(data = world) +
  geom_sf(aes(fill = pop_est)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

#*****************************************************************************
ggplot(data = world) +
  geom_sf() +
  coord_sf(crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ")



#**************************************************************************
install.packages("RCurl")
library(RCurl)

data_abc <- getURL("https://raw.githubusercontent.com/parulnith/Data-Visualisation-with-R/master/Visualisation%20geographical%20data/ABC_locations.csv",
               ssl.verifypeer=0L, followlocation=1L)
writeLines(data_abc,'ABC.csv')
data <- read.csv('ABC.csv')
data
head(data)


plot(data$Longitude,data$Latitude)

# Install package 
install.packages("maps", dependencies=TRUE)
# Loading the installed maps package
library(maps)

map(database="state")
symbols(data$Longitude, data$Latitude, squares =rep(1, length(data$Longitude)), inches=0.03, add=TRUE)
symbols(data$Longitude, data$Latitude,bg = 'red', fg = 'red', squares =rep(1, length(data$Longitude)), inches=0.03, add=TRUE)
#**********************************************************************************



#*************************************3d Visualization*****************************
data(iris)
head(iris)

install.packages("rgl")
library("rgl")


x <- sep.l <- iris$Sepal.Length
y <- pet.l <- iris$Petal.Length
z <- sep.w <- iris$Sepal.Width

rgl.open()# Open a new RGL device
rgl.bg(color = "white") # Setup the background color
rgl.spheres(x, y, z, r = 0.2, color = "grey") 

get_colors <- function(groups, group.col = palette()){
  groups <- as.factor(groups)
  ngrps <- length(levels(groups))
  if(ngrps > length(group.col)) 
    group.col <- rep(group.col, ngrps)
  color <- group.col[as.numeric(groups)]
  names(color) <- as.vector(groups)
  return(color)
}

rgl_init <- function(new.device = FALSE, bg = "white", width = 640) { 
  if( new.device | rgl.cur() == 0 ) {
    rgl.open()
    par3d(windowRect = 50 + c( 0, 0, width, width ) )
    rgl.bg(color = bg )
  }
  rgl.clear(type = c("shapes", "bboxdeco"))
  rgl.viewpoint(theta = 15, phi = 20, zoom = 0.7)
}

rgl_add_axes <- function(x, y, z, axis.col = "grey",
                         xlab = "", ylab="", zlab="", show.plane = TRUE, 
                         show.bbox = FALSE, bbox.col = c("#333377","black"))
{ 
  
  lim <- function(x){c(-max(abs(x)), max(abs(x))) * 1.1}
  # Add axes
  xlim <- lim(x); ylim <- lim(y); zlim <- lim(z)
  rgl.lines(xlim, c(0, 0), c(0, 0), color = axis.col)
  rgl.lines(c(0, 0), ylim, c(0, 0), color = axis.col)
  rgl.lines(c(0, 0), c(0, 0), zlim, color = axis.col)
  
  # Add a point at the end of each axes to specify the direction
  axes <- rbind(c(xlim[2], 0, 0), c(0, ylim[2], 0), 
                c(0, 0, zlim[2]))
  rgl.points(axes, color = axis.col, size = 3)
  
  # Add axis labels
  rgl.texts(axes, text = c(xlab, ylab, zlab), color = axis.col,
            adj = c(0.5, -0.8), size = 2)
  
  # Add plane
  if(show.plane) 
    xlim <- xlim/1.1; zlim <- zlim /1.1
  rgl.quads( x = rep(xlim, each = 2), y = c(0, 0, 0, 0),
             z = c(zlim[1], zlim[2], zlim[2], zlim[1]))
  
  # Add bounding box decoration
  if(show.bbox){
    rgl.bbox(color=c(bbox.col[1],bbox.col[2]), alpha = 0.5, 
             emission=bbox.col[1], specular=bbox.col[1], shininess=5, 
             xlen = 3, ylen = 3, zlen = 3) 
  }
}





library("RColorBrewer")
cols <- get_colors(iris$Species, brewer.pal(n=3, name="Dark2") )
rgl_init()
rgl.spheres(x, y, z, r = 0.2, color = cols) 
rgl_add_axes(x, y, z, show.bbox = TRUE)
aspect3d(1,1,1)




