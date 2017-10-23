library(ggplot2)
# Color the dot plot accoording to the groupName "dose"
ggplot2.dotplot(data=df, xName='dose',yName='len', groupName='dose')
#Change group colors using hexadecimal colors
ggplot2.dotplot(data=df, xName='dose',yName='len', groupName='dose',
                groupColors=c('#999999','#E69F00','#56B4E9'))
# Customized dot plot, add box plot, pink fill color.
ggplot2.dotplot(data=df, xName='dose',yName='len', groupName='dose',
    groupColors=c('#999999','#E69F00','#56B4E9'), showLegend=FALSE,
    backgroundColor="white", xtitle="Dose (mg)", ytitle="length", 
    mainTitle="Plot of length \n by dose",
    addBoxplot=TRUE, boxplotFill="pink")
# Customized dot plot, add box plot, 
#fill box plot accoording to the groups.
ggplot2.dotplot(data=df, xName='dose',yName='len', groupName='dose',
    groupColors=c('#999999','#E69F00','#56B4E9'), showLegend=FALSE,
    backgroundColor="white", xtitle="Dose (mg)", ytitle="length", 
    mainTitle="Plot of length \n by dose",
    addBoxplot=TRUE, boxplotFill=NULL