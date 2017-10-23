 args = commandArgs(T)
library(mime)
library(markdown)
library(beanplot)
library(illuminaio)
library(nor1mix)
library(siggenes)
library(Biobase)
library(BiocGenerics)
library(parallel)
library(multtest)
library(splines)
library(stats)
library(base)
library(Biobase)
library(lattice)
library(reshape)
library(GenomicRanges)
library(IRanges)
library(XVector)
library(Biostrings)
library(foreach)
library(iterators)
library(locfit)
library(minfi)
library(bumphunter)
library(IlluminaHumanMethylationEPICmanifest)
library(IlluminaHumanMethylationEPICanno.ilm10b2.hg19) 
#source("http://bioconductor.org/biocLite.R")
#biocLite("IlluminaHumanMethylationEPICanno.ilm10b2.hg19")
#### get the dir of filename ####
baseDir = getwd()
dir.create(paste(getwd(),"1_Data_QC",sep="/"))
dir.create(paste(getwd(),"2_Preprocess_data",sep="/"))
dir.create(paste(getwd(),"3_Dif_methylation",sep="/"))
######input data 

baseDir = getwd()
######RGset <- read.metharray.exp(file.path(baseDir, "200483200014"))
file.table = read.metharray.sheet(baseDir)  #读取csv工作表
file.table
exp<-read.metharray.exp(target = file.table,force=TRUE)
exp
pd <- pData(exp)
pd
MSet.raw <- preprocessRaw(exp)
###preprocess2 Subset-quantile Within Array Normalisation (SWAN) for nfinium I and II type probes on a single array to be normalized together.
MSet.swan <- preprocessSWAN(exp, MSet.raw)
 
 plotMByType<-function (data, probeTypes = NULL, legendPos = "top", colors = c("black", 
    "red", "blue"), main = "", lwd = 3, cex.legend = 1) 
{
    if (is(data, "MethylSet")) {
        if (ncol(data) > 1) {
            stop("'data' must only contain one sample")
        }
        else {
            betas <- matrix(getM(data), ncol = 1, dimnames = list(rownames(data), 
                colnames(data)))
        }
        if (is.null(probeTypes)) {
            typeI <- getProbeInfo(data, type = "I")[, c("Name", 
                "nCpG")]
            typeII <- getProbeInfo(data, type = "II")[, c("Name", 
                "nCpG")]
            probeTypes <- rbind(typeI, typeII)
            probeTypes$Type <- rep(c("I", "II"), times = c(nrow(typeI), 
                nrow(typeII)))
        }
    }
    else {
        if (is.vector(data) | is.matrix(data)) {
            r = range(data, na.rm = TRUE)
            if (!(r[1] >= 0 & r[2] <= 1)) {
                stop("'data' needs to be a 'MethylSet' or a matrix or vector of beta values")
            }
            else {
                if (is.vector(data)) {
                  betas <- matrix(data, ncol = 1, dimnames = list(names(data), 
                    NULL))
                }
                else {
                  if (ncol(data) > 1) {
                    stop("'data' must only contain one sample")
                  }
                  else {
                    betas <- matrix(data, ncol = 1, dimnames = list(rownames(data), 
                      colnames(data)))
                  }
                }
            }
        }
        else {
            stop("'data' needs to be either a 'MethylSet', 'vector', 'matrix' or 'data.frame'")
        }
    }
    if (!all(c("Name", "Type") %in% colnames(probeTypes))) {
        stop("'probeTypes' must be a data.frame with a column 'Name' of probe IDs and a column 'Type' indicating their design type")
    }
    ymax <- max(sapply(1:ncol(betas), function(x) max(density(betas[, 
        x], na.rm = TRUE)$y)))
    betas <- matrix(betas[!is.na(betas), ], ncol = 1, dimnames = list(rownames(betas)[!is.na(betas)], 
        colnames(betas)))
    total <- nrow(betas)
    type1 <- sum(probeTypes$Type == "I" & probeTypes$Name %in% 
        rownames(betas))
    type2 <- sum(probeTypes$Type == "II" & probeTypes$Name %in% 
        rownames(betas))
    plot(density(betas), main = main, xlab = "M values", ylim = c(0, 
        ymax), lwd = lwd, col = colors[1])
    lines(suppressWarnings(density(betas[rownames(betas) %in% 
        probeTypes$Name[probeTypes$Type == "I"]], weights = rep(1/total, 
        type1))), col = colors[2], lty = 2, lwd = lwd)
    lines(suppressWarnings(density(betas[rownames(betas) %in% 
        probeTypes$Name[probeTypes$Type == "II"]], weights = rep(1/total, 
        type2))), col = colors[3], lty = 2, lwd = lwd)
    legend("topleft", c("All probes", "Infinium I", "Infinium II"), 
        col = colors, lwd = lwd, bg = "white", cex = 0.6, 
        lty = c(1, 2, 2))
}

pdf("1_Data_QC/preprocess_M_compare.pdf")
par(mfrow=c(1,2))
d =  plotMByType(MSet.raw[,1], main = "Raw")
e =  plotMByType(MSet.swan[,1], main = "SWAN")
print(d)
print(e)
dev.off()