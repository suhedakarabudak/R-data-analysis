library(tidyr)
library(plotly)
library(readxl)

#tidyr paketi
gatherfunc<-gather(mydata,key="Odanin enlemi",value="odanin boylami",8:9)
gatherfunc<-unite(gatherfunc,"yeni",neighbourhood_group,neighbourhood,sep="/")

gatherfunc<-separate(gatherfunc,yeni, c("neighbourhood_group","neighbourhood"),sep="/")

df1<- mydata %>%
  replace_na(list(reviews_per_month="bilinmiyor"))
#mboost paketi
confint .mboost 
data <- data.frame(x1 = j, x2 = k ,
                   z = factor(sample(1:3, 500, replace = TRUE)))
data$y <- rnorm(500, mean = data$x1 - data$x2 - 1 * (data$z == 2) +
                  1 * (data$z == 3), sd = 0.1)
linmod <- glmboost(y ~ x1 + x2 + z, data = data,
                   control = boost_control(mstop = 1000))

CI <- confint(linmod, B = 10, level = 0.9)
CI
print(CI, level = 0.9)
## or print a subset (with point estimates):
print(CI, level = 0.8, pe = TRUE, which = "z")
set.seed(1907)
j=mydata$Number_of_rewiews
k=
  data <- data.frame(x1  = j  , x2 =  k
                     data$y <- rnorm(100, mean = data$x1^2 - sin(data$x2), sd = 0.1)
                     gam <- gamboost(y ~ x1 + x2, data = data,
                                     control = boost_control(mstop = 200))
                     
                     CI_gam <- confint(gam, B = 10, level = 0.9)
                     
                     par(mfrow = c(1, 2)) 
                     plot(CI_gam, which = 1)
                     plot(CI_gam, which = 2)
                     
                     lines(CI_gam, which = 2, level = 0.8)
                     }
fp kesirli polinamlar
model <- glmboost(reviews_per_month ~ ., data = mydata, center = TRUE)

maic <- AIC(model)
maic

par(mai = par("mai") * c(1, 1, 1, 1.8))
plot(model)
abline(v = mstop(maic), col = "lightgray")
cv10f <- cv(model.weights(model), type = "kfold")
cvm <- cvrisk(model, folds = cv10f, papply = lapply)
print(cvm)
mstop(cvm)
plot(cvm)
cv10f <- cv(model.weights(model), type = "kfold")
cvm <- cvrisk(model, folds = cv10f, papply = lapply)
print(cvm)
mstop(cvm)
plot(cvm)
Blackboost
xxmydata <- blackboost(reviews_per_month ~ price, data = mydata,
                       control = boost_control(mstop = 50))
xxmydata
plot(reviews_per_month ~ price, data = mydata)
Grups linear effects
x1 <- mydata$reviews_per_month
x2 <- mydata$price
x1 <- scale(x1, center = TRUE, scale = FALSE)
x2 <- scale(x2, center = TRUE, scale = FALSE)
model <- gamboost(y ~ bols(x1, x2, intercept = FALSE) +
                    bols(x1, intercept = FALSE) +
                    bols(x2, intercept = FALSE),
                  control = boost_control(mstop = 50))
coef(model, which = 1)   # one base-learner for x1 and x2
coef(model, which = 2:3) # two separate base-learners for x1 and x2
Laplace
cf <- coef(cars.gb, off2int = TRUE)     ## add offset to intercept
coef(cars.gb) + c(cars.gb$offset, 0)    ## add offset to intercept (by hand)
signif(cf, 3)
signif(coef(lm(mydata$reviews_per_month ~  mydata$price, data = cars)), 3)

cars.gbl <- glmboost( mydata$reviews_per_month ~  mydata$price , data = mydata,
                      control = boost_control(mstop = 1000),
                      family = Laplace())
cars.gbl
coef(cars.gbl, off2int = TRUE)

#plotly paketi 
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray95"),
  subunitcolor = toRGB("gray85"),
  countrycolor = toRGB("gray85"),
  countrywidth = 0.5,
  subunitwidth = 0.5
)

grafik <- plot_geo(mydata, lat = ~latitude, lon = ~longitude)
grafik <- grafik %>% add_markers(
  text = ~paste(name, neighbourhood_group,neighbourhood, paste("Arrivals:",number_of_reviews)),
  color = ~number_of_reviews, symbol = I("square"), size = I(8), hoverinfo = "text"
)
grafik <- grafik%>% colorbar(title = "ilanin incelenme sayisi")
grafik <- grafik%>% layout(
  title = 'Bolgelere gore ilanlarin incelenme sayisi<br />(Incelemek için ustune tiklayin)', geo = g
)

grafik



#2.grafik
df<-mydata[which(mydata$neighbourhood_group=="Brooklyn"),]
grafik<-plot_ly(df,type='pie',labels= ~neighbourhood,
                values= ~price,
                textposition='inside')
grafik<-grafik %>%layout(uniformtext=list(minsize=12,mode='hide'),
                         title="Brooklyn ilçesinin Mahallerine Gore Oda/Ev ucreti")

#3.grafik

plot_ly(mydata,x= ~last_review,
        y= ~number_of_reviews,
        text= ~paste(mydata$calculated_host_listings_count,mydata$number_of_reviews),
        size= ~as.numeric(mydata$last_review),
        type='scatter',
        mode='markers',
        marker=list(size= ~number_of_reviews))
#4.grafik
g1<- plot_ly(data=mydata, x=~neighbourhood_group, y=~price, 
             type = 'bar',
             color = 'orange')
g1<- g1 %>% layout(title= "Odaların Bulunduğu İlçelere Göre Ücret Aralığı",
                   xaxis=list(title= "İlçeler"),
                   yaxis=list(title= "Ücretler"))
g1
#knitr paketi
Knit_rd 

knit_rd("tidyr")
knit_rd("mboost")
setwd(system.file("html", package = "plotly"))
knit_rd("plotly")

#Fig path

fig_path(".pdf", options = list(fig.path = "plot(cvm)", label = "first-plot"))
fig_path(".png", list(fig.path = "plot(cvm)", label = "bar"), 1:10)


#Fig chunk

fig_chunk("plot(cvm)", "png")
fig_chunk("plot(cvm)", "pdf")
fig_chunk("plot(cvm)", "svg", 2)  # the second plot of the chunk foo
fig_chunk("plot(cvm)", "png", 1:5)

#All paterns 

all_patterns$x
all_patterns$html

str(all_patterns)

Knitr::kable 

mydata3 <- head(mydata)

knitr::kable(mydata3, col.names = gsub("[.]", " ", names(mydata)))