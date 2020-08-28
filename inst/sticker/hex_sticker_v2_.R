#
# Author; Emanuele Cordano
# Date: 2020-08-13
#
library(hexSticker)
library(png)
library(magrittr)
library(purrr)

imgurl <- system.file("figures/cat.png", package="hexSticker")
imggeotop <- '/home/ecor/Dropbox/R-packages/geotopbricks/inst/sticker/logo/logo_geotop.png'

##

filesticker <- '/home/ecor/Dropbox/R-packages/geotopbricks/inst/sticker/sticker_geotopbricks_v2_prov_.png'
filesticker3 <- '/home/ecor/Dropbox/R-packages/geotopbricks/inst/sticker/sticker_geotopbricks_v2_.png'
filesticker3_jpg  <- '/home/ecor/Dropbox/R-packages/geotopbricks/inst/sticker/sticker_geotopbricks_v2_.jpg'
h_color <- "#1881C2"
p_color <- "#1881C2"
u_color <- rgb(237+5,171+5,37+2,maxColorValue = 255) ##rgb(218,187,55,maxColorValue = 255) ##rgb(237,171,37,maxColorValue = 255) ##"#ffed6f" ## "yellow" ####eff3ff" ##white" ## #005824" ## "#01665e"
sticker(imggeotop, package="geotopbricks",url="www.geotop.org", p_size=40, s_x=1, s_y=0.8, s_width=.4,
        filename=filesticker,u_size=18,h_color=h_color,h_fill="white",u_color=u_color,p_color=p_color,dpi=600)



rgbn <- c("red","green","blue","alpha")
library(raster)


out <- stack(filesticker)

plotRGB(out)
names(out)[1:4] <- rgbn
bg_source <- '/home/ecor/Dropbox/R-packages/geotopbricks/inst/sticker/logo/PA120471.png'    
bg <- stack(bg_source) 

bg[[4]] <- 255
## AGGREGRATION: DECRESE RESOLUTION 
names(bg) <- rgbn
fact <- 2

bg <- aggregate(bg,fact=fact,fun=compose(as.integer,mean))
offset <- -c(1000,150)
extent(bg) <- as.vector(extent(bg))/fact+offset[c(1,1,2,2)]
bg_c <- crop(bg,out)

p <- 2 ##2
white <- ((out$red-255)^p+(out$green-255)^p+(out$blue-255)^p)^(1/p)
white <- (white-min(white[],na.rm=TRUE))/(max(white[],na.rm=TRUE)-min(white[],na.rm=TRUE))
white <- white^(1/2) ###(1-(white<=0.02))


##out3 <- merge(out,out2)
##names(out3) <- rgbn

hc <- (out$red==col2rgb(h_color)["red",1]) & (out$green==col2rgb(h_color)["green",1]) &(out$blue==col2rgb(h_color)["blue",1])
####white <- white | hc 
out2 <- bg_c*(1-white)+white*out
names(out2) <- names(out)
out3 <- out2*(1-hc)+out*hc
##out3[hc] <- out[hc]  
names(out3) <- names(out)
plotRGB(out3)
out4 <- array(NA,dim(out3))


for (i in 1:nlayers(out3)) {
  
  out4[,,i] <- as.matrix(out3[[i]])/255
}
writePNG(out4,target=filesticker3,dpi=300)
## END 
library(jpeg)
writeJPEG(out4,target=filesticker3_jpg,quality=1)
