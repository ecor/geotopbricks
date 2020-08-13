
#
# Author; Emanuele Cordano
# Date: 2020-07-11 / 2020-08-13
#
library(hexSticker)
library(png)
library(magrittr)


imggeotop <- '/home/ecor/Dropbox/R-packages/geotopbricks/inst/sticker/logo/logo_geotop.png'
filesticker <- '/home/ecor/Dropbox/R-packages/geotopbricks/inst/sticker/sticker_geotopbricks_v1.png'
sticker(imggeotop, package="geotopbricks",url="www.geotop.org", p_size=20, s_x=1, s_y=0.8, s_width=.4,
        filename=filesticker,u_size=9,h_color="#1881C2",h_fill="white",u_color="#01665e",p_color="#1881C2")


library(raster)
plotRGB(stack(filesticker))