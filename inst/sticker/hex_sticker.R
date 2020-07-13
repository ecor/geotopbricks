#
# Author; Emanuele Cordano
# Date: 2020-07-11
#
library(hexSticker)
library(png)
library(magrittr)
###https://github.com/GuangchuangYu/hexSticker
imgurl <- system.file("figures/cat.png", package="hexSticker")
imgin <- '/home/ecor/Dropbox/R-packages/geotopbricks/inst/sticker/logo/logo_geotop.png'
###imgin_cut <- '/home/ecor/Dropbox/R-packages/geotopbricks/inst/sticker/logo/logo_geotop_cut.png'
##logor <- '/home/ecor/Dropbox/R-packages/geotopbricks/inst/sticker/logo/logo_r.png'
mimgin <- imgin %>% readPNG()
rhold <- 1:300
mimgin_cut <- mimgin ##[rhold,,]
writePNG(mimgin_cut,imgin_cut)
img <- readPNG(system.file("img","Rlogo.png",package="png"))

##

filesticker <- '/home/ecor/Dropbox/R-packages/geotopbricks/inst/sticker/sticker_geotopbricks.png'

  
# sticker(imgin, package="geotopbricks",url="www.geotop.org", p_size=20, s_x=1, s_y=0.8, s_width=.4,
#         filename=filesticker,u_size=9,h_color="white",h_fill="#1881C2",u_color="#01665e") #2c7fb8


sticker(imgin_cut, package="geotopbricks",url="www.geotop.org", p_size=20, s_x=1, s_y=0.8, s_width=.4,
        filename=filesticker,u_size=9,h_color="#1881C2",h_fill="white",u_color="#01665e",p_color="#1881C2")

