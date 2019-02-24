#*****************************************************************
# Modified functions in EchoR to have the right colours, names, etc...
# (run before EchoR)
#*****************************************************************
# Acoustic fish biomass assessment, Ifremer methodology 
#
# created by @Silvia Rodriguez Climent
#*****************************************************************
#*****************************************************************

#Run it before step 2 (script 2) for the right colours
# 1.show.esdu.refhaul2----
show.esdu.refhaul2=
  
  function (Ndevi, list.Associ, Pechelsi, Pechei, EsduDevi, 
            dcol = c(2,4, 1, 3, 7, 8, 6, 5), lcol = NULL, legpos = "topright", add.layer = NULL, 
            export.plot = NULL, radSA = 0.2, pradius = 1, scale.SA = 1, 
            xlim = NULL, ylim = NULL, smax = 1, corr = cos(46.5 * pi/180), 
            logit = FALSE, labelit = FALSE, nid = "NOCHAL", cex.lab = 1, 
            ux11 = TRUE) 
  {
    library(maptools)
    lsps = names(Pechelsi)[!names(Pechelsi) %in% c("CAMPAGNE", 
                                                   "NOCHAL", "NOSTA", "LONF", "LATF", "STRATE", "SONDE", 
                                                   "GEAR")]
    lsps2 = substr(lsps, 4, 11)
    if (is.null(lcol)) {
      spcol = data.frame(sp = c("ENGR.ENC", "MICR.POU", "SARD.PIL", 
                                "SCOM.SCO", "SPRA.SPR", "TRAC.TRA", "CAPR.APE", "SCOM.JAP", 
                                "TRAC.MED", "CLUP.HAR", "COMP.LEM", "MERL.MNG", "MERL.MER"), 
                         sp2 = c("ENGR-ENC", "MICR-POU", "SARD-PIL", "SCOM-SCO", 
                                 "SPRA-SPR", "TRAC-TRA", "CAPR-APE", "SCOM-JAP", 
                                 "TRAC-MED", "CLUP-HAR", "COMP-LEM", "MERL-MNG", 
                                 "MERL-MER"), lcol = c(3, 8, 4, 2, 1, 7, 6, 5, 0, 0, 0, 0, 0))
      
      lcol = spcol[, "lcol"][match(lsps2, spcol$sp)]
    }
    for (i in 1:length(Ndevi)) {
      Di = Ndevi[i]
      lai = list.Associ[[i]]
      lnochai = unique(lai$NOCHAL)
      Pechelsis = Pechelsi[Pechelsi[, nid] %in% lnochai, ]
      alls = (lnochai %in% Pechelsi[, nid])
      if (FALSE %in% alls) {
        cat("Dev", i, "Haul", lnochai[!alls], "has no valid geographic position", 
            "\n")
      }
      esduch = merge(lai, unique(Pechei[, c(nid, "LONF", "LATF")]), 
                     by.x = "NOCHAL", by.y = nid)
      esduch = merge(esduch, unique(EsduDevi[, c("esdu.id", 
                                                 "LONG", "LAT", substr(Ndevi[i], 1, 2))]), by.x = "Esdu", 
                     by.y = "esdu.id")
      if (sum(!is.na(esduch$Dev)) == 0) {
        esduch$Dev = esduch[, substr(Ndevi[i], 1, 2)]
      }
      esduch.nn = esduch[esduch$Dev != 0, ]
      if (!is.null(export.plot)) {
        png(file = paste(export.plot, "devCH-", Di, ".png", 
                         sep = ""), width = 800, height = 800)
      }
      else if (ux11) 
        x11()
      par(mar = c(2, 2, 3, 1))
      xrb = c(EsduDevi$LONG, Pechelsis$LONF)
      yrb = c(EsduDevi$LAT, Pechelsis$LATF)
      if (is.null(xlim)) {
        xlim = c(min(xrb) - abs(max(xrb) - min(xrb))/10, 
                 max(xrb) + abs(max(xrb) - min(xrb))/10)
      }
      Esdus = EsduDevi[EsduDevi$LONG >= xlim[1] & EsduDevi$LONG <= 
                         xlim[2], ]
      Pecheis = Pechelsis[Pechelsis$LONF >= xlim[1] & Pechelsis$LONF <= 
                            xlim[2], ]
      if (is.null(ylim)) {
        ylim = c(min(yrb) - abs(max(yrb) - min(yrb))/10, 
                 max(yrb) + abs(max(yrb) - min(yrb))/10)
      }
      Esdus = Esdus[Esdus$LAT >= ylim[1] & Esdus$LAT <= ylim[2], 
                    ]
      Pecheis = Pecheis[Pecheis$LATF >= ylim[1] & Pecheis$LATF <= 
                          ylim[2], ]
      if (!Di %in% names(Esdus)) {
        sa = rep(0.1, dim(Esdus)[1])
      }
      else {
        sa = Esdus[, Di]
      }
      plot(Esdus$LONG, Esdus$LAT, cex = radSA + log(sa + 1)/scale.SA, 
           pch = 1, col = "grey50", main = Di, xlim = xlim, 
           ylim = ylim)
      coast()
      if (dim(esduch.nn)[1] > 0) {
        segments(esduch.nn$LONG, esduch.nn$LAT, esduch.nn$LONF, 
                 esduch.nn$LATF, col = 1)
        if (dim(Pecheis)[1] > 0) {
          if (logit) {
            z = as.matrix(log(Pecheis[, lsps] + 1))
          }
          else {
            z = as.matrix(Pecheis[, lsps])
          }
          pie.xy(x = Pecheis$LONF, y = Pecheis$LATF, z = z, 
                 pcoast = FALSE, pcol = lcol, draw1 = FALSE, 
                 pradius = pradius, smax = smax)
          legend(legpos, legend = substr(lsps, 4, 11), 
                 fill = lcol, bg = "white")
          if (labelit) 
            text(Pecheis$LONF, y = Pecheis$LATF, Pecheis[, 
                                                         nid], cex = cex.lab, col = "grey50")
          if (!is.null(add.layer)) 
            plot(add.layer, add = TRUE, col = 2)
        }
      }
      else mtext("All esdus associated to hauls with no geographic position")
      if (!is.null(export.plot)) {
        cat("Haul-esdu association plot saved to:", export.plot, 
            "\n")
        dev.off()
      }
    }
  }

#*****************************************************************
#2.Bdev.sp.plot2----
#Run it before step 17 (script 3) for the right colours
Bdev.sp.plot2=
  
  function (k, biom.esdu.dev.sp, Ndev, save.plot = FALSE, path.res.charef = NULL, 
            sp.sel = FALSE, logit = FALSE, smax = 1, plotit = TRUE, legpos = "bottomleft", 
            ux11 = FALSE, pradius = 1) 
  {
    devi = paste("D", Ndev[k], sep = "")
    biom.D1 = biom.esdu.dev.sp[biom.esdu.dev.sp$DEV == paste("D", 
                                                             Ndev[k], sep = ""), ]
    head(biom.D1)
    biom.Dn = biom.D1[, !names(biom.D1) %in% c("Esdu", "DEV", 
                                               "CAMPAGNE", "charef", "TC", "LAT", "LONG")]
    apply(biom.Dn, 2, sum)
    lspz = substr(names(biom.Dn), 1, 13)
    luspz = unique(lspz)
    for (i in 1:length(luspz)) {
      bioms = biom.Dn[, lspz == luspz[i]]
      if (!is.null(dim(bioms))) {
        slspz = data.frame(rowSums(bioms))
      }
      else {
        slspz = data.frame(bioms)
      }
      names(slspz) = luspz[i]
      if (i == 1) {
        biom.D1.1z = data.frame(Esdu = biom.D1$Esdu, slspz)
      }
      else {
        biom.D1.1z = data.frame(biom.D1.1z, slspz)
      }
    }
    names(biom.D1.1z)
    head(biom.D1.1z)
    Btot.D1 = apply(biom.D1.1z[, -1], 2, sum)
    Btot.D1s = Btot.D1[Btot.D1 > 0]
    Btot.D1l = data.frame(CodEsp = names(Btot.D1s), Btot = Btot.D1s)
    Btot.D1l$CodEsp = gsub("\\.", "-", Btot.D1l$CodEsp)
    Btot.D1l[, "GENR_ESP"] = substr(Btot.D1l$CodEsp, 1, 8)
    Btot.D1l$DEV = devi
    Btot.D1l$CAMPAGNE = unique(biom.D1$CAMPAGNE)
    biom.D1s = data.frame(biom.D1.1z[, c(TRUE, Btot.D1 > 0)])
    names(biom.D1s)
    dim(biom.D1s)
    biom.D1s = merge(biom.D1[, c("TC", "Esdu", "LAT", "LONG")], 
                     biom.D1s, by.x = "Esdu", by.y = "Esdu")
    dim(biom.D1s)
    head(biom.D1s)
    lsp2 = names(biom.D1s)[c(5:dim(biom.D1s)[2])]
    if (sp.sel) {
      biom.D1s = biom.D1s[, c(rep(TRUE, 4), !substr(lsp2, 1, 
                                                    4) %in% "SCOM")]
      lsp2 = lsp2[!substr(lsp2, 1, 4) %in% "SCOM"]
    }
    cols = data.frame(sp = lsp2, sp2 = substr(lsp2, 1, 8))
    spcol.db = data.frame(sp = c("ENGR.ENC", "MICR.POU", "SARD.PIL", 
                                 "SCOM.SCO", "SCOM.JAP", "SPRA.SPR", "TRAC.TRA", "TRAC.MED", 
                                 "CAPR.APE"), lcol = c(3, 8, 4, 2, 5, 1, 7, 6, 6))
    
    spcol = merge(cols, spcol.db, by.x = "sp2", by.y = "sp")
    spcol = spcol[match(lsp2, spcol$sp), ]
    pcols = spcol$lcol
    if (plotit & save.plot & !is.null(path.res.charef)) {
      if (sp.sel) {
        nfile = paste(devi, "_biomassMap-woMACK~expert.png", 
                      sep = "")
      }
      else {
        nfile = paste(devi, "_biomassMap~expert.png", sep = "")
      }
      png(file = paste(path.res.charef, nfile, sep = ""), width = 600, 
          height = 600)
      if (logit) {
        z = log(as.matrix(biom.D1s[, lsp2]) + 1)
        mtitle = paste(unique(biom.D1$CAMPAGNE), devi, "log(biomass+1) per species")
      }
      else {
        z = as.matrix(biom.D1s[, lsp2])
        mtitle = paste(unique(biom.D1$CAMPAGNE), devi, "biomass per species")
      }
      if (plotit) {
        pie.xy(x = biom.D1s$LONG, y = biom.D1s$LAT, z = z, 
               pcol = pcols, mtitle = mtitle, smax = smax, bar = FALSE, 
               pradius = pradius, ux11 = ux11)
        legend(legpos, legend = lsp2, fill = pcols, bg = "white")
        dev.off()
      }
    }
    if ((plotit & k > 1) & ux11) {
      x11()
    }
    if (logit) {
      z = log(as.matrix(biom.D1s[, lsp2]) + 1)
      mtitle = paste(unique(biom.D1$CAMPAGNE), devi, "log(biomass+1) per species")
    }
    else {
      z = as.matrix(biom.D1s[, lsp2])
      mtitle = paste(unique(biom.D1$CAMPAGNE), devi, "biomass per species")
    }
    if (plotit) {
      pie.xy(x = biom.D1s$LONG, y = biom.D1s$LAT, z = z, pcol = pcols, 
             mtitle = mtitle, smax = smax, bar = FALSE, pradius = pradius, 
             ux11 = ux11)
      legend(legpos, legend = lsp2, fill = pcols, bg = "white")
    }
    Btot.D1l
  }
#*****************************************************************
#lcol = c(3, 8, 4, 2, 5, 1, 7, 6, 6))
#lcol=c("green","white","blue","red","white","black","yellow","white","orange")
#*****************************************************************

#*****************************************************************
#3.areahull2----
#Run before script 3, for the correct area calculation
# Warning message:
#   'evalWithTimeout' is deprecated.
# Use 'withTimeout' instead.
# See help("Deprecated") 

areaahull2=
  
  function (x, timeout = 5) 
  {
    area <- withTimeout(try(areaahulleval(x), silent = TRUE), 
                        timeout = timeout)
    if (!is.numeric(area)) {
      warning("Problem in area computation (Returns NA)")
      area <- NA
    }
    if (is.numeric(area) & area < 0) {
      warning("Problem in area computation (Returns NA)")
      area <- NA
    }
    return(area)
  }



#*****************************************************************
#4.Biomass.esdu.check2_Function modified to save the plots as they are computed-----
#run before script3

biomass.esdu.check2=
  
  function (B.dev.sp.esdu.df, saveIt = TRUE, plotIt = TRUE, path.results = NULL, 
            Pechef) 
  {
    if (plotIt) {
      par(mfrow = c(1, 1))
      simple.1sp.dev.esdu.plot2(B.dev.sp.esdu.df)
    }
    colSums(B.dev.sp.esdu.df[B.dev.sp.esdu.df$BN == 0, c("BB", 
                                                         "BN")])
    colSums(B.dev.sp.esdu.df[B.dev.sp.esdu.df$BB == 0, c("BB", 
                                                         "BN")])
    Btot.sp.dev.df = aggregate(B.dev.sp.esdu.df[, c("BB", "BN")], 
                               list(B.dev.sp.esdu.df$CAMPAGNE, B.dev.sp.esdu.df$DEV, 
                                    B.dev.sp.esdu.df$CodEsp, B.dev.sp.esdu.df$GENR_ESP), 
                               sum, na.rm = TRUE)
    names(Btot.sp.dev.df) = c("CAMPAGNE", "DEV", "CodEsp", "sp", 
                              "BB", "BN")
    cat("Total biomass and abundance per species code and echotype", 
        "\n")
    print(Btot.sp.dev.df)
    B.dev.sp.esdu.df.BNna = B.dev.sp.esdu.df[is.na(B.dev.sp.esdu.df$BN), 
                                             ]
    B.dev.sp.esdu.df[is.na(B.dev.sp.esdu.df$BN), "BN"] = 0
    Btot.sp.df = aggregate(B.dev.sp.esdu.df[, c("BB", "BN")], 
                           list(B.dev.sp.esdu.df$CAMPAGNE, B.dev.sp.esdu.df$GENR_ESP), 
                           sum, na.rm = TRUE)
    names(Btot.sp.df) = c("CAMPAGNE", "sp", "BB", "BN")
    cat("Total biomass and abundance per species", "\n")
    print(Btot.sp.df)
    mB.csp.dev.df = aggregate(B.dev.sp.esdu.df[, c("BB", "BN")], 
                              list(B.dev.sp.esdu.df$CAMPAGNE, B.dev.sp.esdu.df$DEV, 
                                   B.dev.sp.esdu.df$CodEsp, B.dev.sp.esdu.df$GENR_ESP), 
                              mean, na.rm = TRUE)
    names(mB.csp.dev.df) = c("CAMPAGNE", "DEV", "CodEsp", "sp", 
                             "mBB", "mBN")
    cat("Mean biomass and abundance per species code and echotype", 
        "\n")
    print(mB.csp.dev.df)
    mB.csp.df = aggregate(mB.csp.dev.df[, c("mBB", "mBN")], list(mB.csp.dev.df$CAMPAGNE, 
                                                                 mB.csp.dev.df$CodEsp, mB.csp.dev.df$sp), sum, na.rm = TRUE)
    names(mB.csp.df) = c("CAMPAGNE", "CodEsp", "sp", "mBB", "mBN")
    cat("Total mean biomass and abundance per species code", 
        "\n")
    print(mB.csp.df)
    mB.sp.df = aggregate(mB.csp.df[, c("mBB", "mBN")], list(mB.csp.df$CAMPAGNE, 
                                                            mB.csp.df$sp), sum, na.rm = TRUE)
    names(mB.sp.df) = c("CAMPAGNE", "sp", "mBB", "mBN")
    Btot.sp.df$mw.g = Btot.sp.df$BB * 1e+06/Btot.sp.df$BN
    mw.peche = aggregate(Pechef$PM, list(Pechef$GENR_ESP), mean, 
                         na.rm = TRUE)
    names(mw.peche) = c("sp", "mw")
    mw.peche$mw.g.catch = mw.peche$mw
    Btot.sp.df = merge(Btot.sp.df, mw.peche[, c("sp", "mw.g.catch")], 
                       by.x = "sp", by.y = "sp")
    Bsum.sp.esdu = merge(Btot.sp.df, mB.sp.df)
    cat("Descriptive stats summary", "\n")
    print(Bsum.sp.esdu)
    if (saveIt & !is.null(path.results)) {
      write.table(Bsum.sp.esdu, paste(path.results, "/Bsum.sp.esdu.csv", sep = ""), sep = ";", row.names = FALSE)
    }
    Bsum.sp.esdu
  }
######-

simple.1sp.dev.esdu.plot2=
  
  function (B.dev.sp.esdu.df) 
  {
    lsp = unique(B.dev.sp.esdu.df$GENR_ESP)
    for (i in 1:length(lsp)) {
      BdevSP = B.dev.sp.esdu.df[B.dev.sp.esdu.df$GENR_ESP == 
                                  lsp[i], ]
      ldev = unique(BdevSP$DEV)
      for (k in 1:length(ldev)) {
        BdevSPk = BdevSP[BdevSP$DEV == ldev[k], ]
        BdevSPk0 = BdevSPk[BdevSPk$BB == 0, ]
        png(paste(path.results.esdu,cruise,lsp[i],"_Besdu.jpg"),width=800,height=800) #added by me
        plot(BdevSPk$LONG, BdevSPk$LAT, cex = 0.1 + BdevSPk$BN/5e+06, 
             main = paste(lsp[i], "Echotype", ldev[k]), xlab = "", ylab = "")
        points(BdevSPk0$LONG, BdevSPk0$LAT, cex = 1, col = 2, 
               pch = 4)
        legend("bottomright", legend = c("Non-null abundance", 
              "Null abundance"), pch = c(1, 4), col = seq(2))
        coast()
        dev.off()
      }
    }
  }



#*****************************************************************
#5.B.esdu.size.so.plot2----
#Run before the script 4 to have the right colours
B.esdu.size.sp.plot2=
  
  function (k, list.sp.NW.size, lspcodes.mens, EsduDev, save.plot = FALSE, 
            path.res.charef = NULL, clims = vector("list", 10), logit = FALSE, 
            v2plot = "W", smax = 1, barplotit = TRUE, spname = "CodEsp", 
            plotit = list(Lclass1 = TRUE, LclassALL = TRUE), ux11 = FALSE, 
            cruise = "") 
  {
    spi = lspcodes.mens[k]
    cat(spi, "\n")
    spcols = data.frame(CodEsp = c("MICR-POU-0", "ENGR-ENC-0", 
                                   "SARD-PIL-0", "SCOM-SCO-0", "SPRA-SPR-0", "TRAC-TRA-0", 
                                   "CAPR-APE-0", "TRAC-MED-0", "SCOM-JAP-0"), GENR_ESP = c("MICR-POU", 
                                                                                           "ENGR-ENC", "SARD-PIL", "SCOM-SCO", "SPRA-SPR", "TRAC-TRA", 
                                                                                           "CAPR-APE", "TRAC-MED", "SCOM-JAP"), spcol = c(8, 3, 
                                                                                                                                          4, 2, 1, 7, 6, 6, 9))
    spcol = spcols[spcols[, spname] == spi, "spcol"]
    biom.D1.1z = data.frame(list.sp.NW.size[[k]])
    head(biom.D1.1z)
    Btot.D1 = apply(biom.D1.1z[, -seq(3)], 2, sum)
    Btot.D1s = Btot.D1[Btot.D1 > 0]
    biom.D1s = data.frame(biom.D1.1z[, c(TRUE, TRUE, FALSE, Btot.D1 > 
                                           0)])
    head(biom.D1s)
    dim(biom.D1s)
    biom.D1s = merge(EsduDev[, c("TC", "esdu.id", "LAT", "LONG")], 
                     biom.D1s, by.y = "Esdu", by.x = "esdu.id")
    dim(biom.D1s)
    head(biom.D1s)
    colt = substr(names(biom.D1s)[-seq(5)], 1, 1)
    biom.D1s2 = biom.D1s[, c(rep(TRUE, 5), colt == v2plot)]
    colt2 = substr(names(biom.D1s2), 1, 1)
    Ls = as.numeric(substr((names(biom.D1s2)[colt2 == v2plot]), 
                           3, 6))
    clim = clims[[k]]
    if (is.null(clim)) {
      clim = seq(0, 100, 10)
    }
    cLs = cut(Ls, clim)
    length(cLs)
    twl = t(biom.D1s2[, colt2 == v2plot])
    dim(twl)
    biom.D1s2a = aggregate(twl, by = list(cLs), FUN = sum)
    z = t(biom.D1s2a[, -1])
    cLsi = biom.D1s2a[, 1]
    dimnames(z)[[2]] = cLsi
    tbiom.D1s2a = data.frame(z)
    tbiom.D1s2a$x = biom.D1s$LONG
    tbiom.D1s2a$y = biom.D1s$LAT
    if (length(cLsi) == 1) {
      pcol = c("yellow1")
    }
    else if (length(cLsi) == 2) {
      pcol = c("yellow1", "red")
    }
    else if (length(cLsi) == 3) {
      pcol = c("yellow1", "orange3", "red")
    }
    else if (length(cLsi) == 4) {
      pcol = c("yellow1", "orange2", "indianred", "red")
    }
    if (save.plot & !is.null(path.res.charef)) {
      if (logit) {
        zp = log(z + 1)
        mtitle = paste(spi, ", log(biomass+1) per size class", 
                       sep = "")
      }
      else {
        zp = z
        mtitle = paste(spi, ", biomass per size class", sep = "")
      }
      if (plotit$LclassALL) {
        nfile = paste(cruise, "_", spi, "_biomassMapLength~expert.png", 
                      sep = "")
        png(file = paste(path.res.charef, nfile, sep = ""), 
            width = 600, height = 600)
        pie.xy(x = biom.D1s$LONG, y = biom.D1s$LAT, z = zp, 
               pcol = pcol, mtitle = mtitle, smax = smax, bar = barplotit)
        legend("bottomleft", legend = cLsi, fill = pcol, 
               bg = "white", title = "Length class (cm)")
        dev.off()
      }
      if (plotit$Lclass1) {
        for (i in 1:length(cLsi)) {
          if (ux11) 
            x11()
          par(bg = "white")
          plot(tbiom.D1s2a$x, tbiom.D1s2a$y, asp = 1/cos(46 * 
                                                           pi/180), cex = 0.001, xlab = "", ylab = "", 
               main = paste(spi, cLsi[i], "cm, biomass per EDSU (tons)"), 
               pch = 16, col = "grey70")
          points(tbiom.D1s2a$x, tbiom.D1s2a$y, asp = 1/cos(46 * 
                                                             pi/180), cex = log(tbiom.D1s2a[, i] + 1)/10, 
                 xlab = "", ylab = "", main = paste(spi, cLsi[i], 
                                                    "cm, biomass per EDSU (tons)"), pch = 16, 
                 col = spcol)
          coast()
          nfile = paste(cruise, "_", spi, "_biomassMapLength", 
                        cLsi[i], "~expert.png", sep = "")
          dev.print(device = png, filename = paste(path.res.charef, 
                                                   nfile, sep = ""), width = 800, height = 800)
        }
      }
    }
    else {
      if (logit) {
        zp = log(z + 1)
        mtitle = paste(spi, ", log(biomass+1) per size class", 
                       sep = "")
      }
      else {
        zp = z
        mtitle = paste(spi, ", biomass per size class", sep = "")
      }
      if (plotit$LclassALL) {
        if (ux11) 
          x11()
        pie.xy(x = biom.D1s$LONG, y = biom.D1s$LAT, z = zp, 
               pcol = pcol, mtitle = mtitle, smax = smax, bar = barplotit)
        legend("bottomleft", legend = cLsi, fill = pcol, 
               bg = "white", title = "Length class (cm)")
      }
      if (plotit$Lclass1) {
        for (i in 1:length(cLsi)) {
          if (ux11) 
            x11()
          plot(tbiom.D1s2a$x, tbiom.D1s2a$y, asp = 1/cos(46 * 
                                                           pi/180), cex = 0.001, xlab = "", ylab = "", 
               main = paste(spi, cLsi[i], "cm, biomass per EDSU (tons)"), 
               pch = 16, col = "grey70")
          points(tbiom.D1s2a$x, tbiom.D1s2a$y, asp = 1/cos(46 * 
                                                             pi/180), cex = log(tbiom.D1s2a[, i] + 1)/10, 
                 xlab = "", ylab = "", main = paste(spi, cLsi[i], 
                                                    "cm, biomass per EDSU (tons)"), pch = 16, 
                 col = spcol)
          coast()
        }
      }
    }
  }

#*****************************************************************
#6.catches.piePlot2----
#Run before script 6, step 23.0 (for right colours)

catches.piePlot2=
  
  function (EsduDevi, Pechelsi, Pechei, dcol = c(2, 4, 1, 3, 7, 
                                                 8, 6, 5), lcol = NULL, legpos = "topright", add.layer = NULL, 
            export.plot = NULL, radSA = 0.2, pradius = 1, scale.SA = 1, 
            xlim = NULL, ylim = NULL, smax = 1, corr = cos(46.5 * pi/180), 
            logit = FALSE, labelit = FALSE, nid = "NOCHAL", cex.lab = 1, 
            ux11 = TRUE) 
  {
    library(maptools)
    lsps = names(Pechelsi)[!names(Pechelsi) %in% c("CAMPAGNE", 
                                                   "NOCHAL", "NOSTA", "LONF", "LATF", "STRATE", "SONDE", 
                                                   "GEAR")]
    lsps2 = substr(lsps, 4, 11)
    if (is.null(lcol)) {
      spcol = data.frame(sp = c("ENGR.ENC", "MICR.POU", "SARD.PIL", 
                                "SCOM.SCO", "SPRA.SPR", "TRAC.TRA", "CAPR.APE", "SCOM.JAP", 
                                "TRAC.MED", "CLUP.HAR", "COMP.LEM", "MERL.MNG", "MERL.MER"), 
                         sp2 = c("ENGR-ENC", "MICR-POU", "SARD-PIL", "SCOM-SCO", 
                                 "SPRA-SPR", "TRAC-TRA", "CAPR-APE", "SCOM-JAP", 
                                 "TRAC-MED", "CLUP-HAR", "COMP-LEM", "MERL-MNG", 
                                 "MERL-MER"), lcol = c(3, 8, 4, 2, 1, 7, 6, 5, 
                                                       0, 0, 0, 0, 0))
      lcol = spcol[, "lcol"][match(lsps2, spcol$sp)]
    }
    Pechelsis = Pechelsi
    if (!is.null(export.plot)) {
      png(file = paste(export.plot, cruise, "TotSaHauls.png", 
                       sep = ""), width = 800, height = 800)
    }
    else if (ux11) 
      x11()
    par(mar = c(2, 2, 3, 1))
    xrb = c(EsduDevi$LONG, Pechelsis$LONF)
    yrb = c(EsduDevi$LAT, Pechelsis$LATF)
    if (is.null(xlim)) {
      xlim = c(min(xrb) - abs(max(xrb) - min(xrb))/10, max(xrb) + 
                 abs(max(xrb) - min(xrb))/10)
    }
    Esdus = EsduDevi[EsduDevi$LONG >= xlim[1] & EsduDevi$LONG <= 
                       xlim[2], ]
    Pecheis = Pechelsis[Pechelsis$LONF >= xlim[1] & Pechelsis$LONF <= 
                          xlim[2], ]
    if (is.null(ylim)) {
      ylim = c(min(yrb) - abs(max(yrb) - min(yrb))/10, max(yrb) + 
                 abs(max(yrb) - min(yrb))/10)
    }
    Esdus = Esdus[Esdus$LAT >= ylim[1] & Esdus$LAT <= ylim[2], 
                  ]
    Pecheis = Pecheis[Pecheis$LATF >= ylim[1] & Pecheis$LATF <= 
                        ylim[2], ]
    sa = Esdus[, "TOTAL"]
    plot(Esdus$LONG, Esdus$LAT, cex = radSA + log(sa + 1)/scale.SA, 
         pch = 1, col = "grey50", xlim = xlim, ylim = ylim)
    coast()
    if (dim(Pecheis)[1] > 0) {
      if (logit) {
        z = as.matrix(log(Pecheis[, lsps] + 1))
      }
      else {
        z = as.matrix(Pecheis[, lsps])
      }
      pie.xy(x = Pecheis$LONF, y = Pecheis$LATF, z = z, pcoast = FALSE, 
             pcol = lcol, draw1 = FALSE, pradius = pradius, smax = smax)
      legend(legpos, legend = substr(lsps, 4, 11), fill = lcol, 
             bg = "white")
      if (labelit) 
        text(Pecheis$LONF, y = Pecheis$LATF, Pecheis[, nid], 
             cex = cex.lab, col = "grey50")
      if (!is.null(add.layer)) 
        plot(add.layer, add = TRUE, col = 2)
    }
    if (!is.null(export.plot)) {
      cat("Catches pie plot saved to:", export.plot, "\n")
      dev.off()
    }
  }

#*****************************************************************
#7.CV estimation for all the assessed species in PELTIC-----
EVA.barplot2=
  
  function (df, lsps = c("SCOM-SCO", "TRAC-TRA", "ENGR-ENC", "SARD-PIL","CAPR-APE","SPRA-SPR"), 
            anames = NULL, vnames = c("prop.vara", "prop.varw"), v2names = NULL, 
            tlegend = c("Weighted mean Xe", "Arithmetic mean Xe"), xlabs = c("", 
                                                                             ""), mar = c(5, 4, 4, 2) + 0.1, ...) 
  {
    CV.dfs = df[df$sp %in% lsps, ]
    lsp = unique(CV.dfs$sp)
    if (length(lsps) == 1) {
      par(mfrow = c(1, 2), mar = mar)
      barplot(t(as.matrix(CV.dfs[, vnames])), names.arg = CV.dfs[, 
                                                                 anames], horiz = TRUE, las = 2, main = lsp[1], beside = TRUE, 
              legend = tlegend, xlab = xlabs[1], ...)
      barplot(t(as.matrix(CV.dfs[, v2names])), names.arg = CV.dfs[, 
                                                                  anames], horiz = TRUE, las = 2, main = lsp[1], beside = TRUE, 
              xlab = xlabs[2])
    }
    else {
      par(mfrow = c(2, 2), mar = mar)
      for (i in 1:length(lsp)) {
        CV.dfi = CV.dfs[CV.dfs$sp == lsp[i], ]
        if (i == 1) {
          barplot(t(as.matrix(CV.dfi[, vnames])), names.arg = CV.dfi[, 
                                                                     anames], horiz = TRUE, las = 2, main = lsp[i], 
                  beside = TRUE, legend = tlegend, xlab = xlabs[1], 
                  ...)
        }
        else {
          barplot(t(as.matrix(CV.dfi[, vnames])), names.arg = CV.dfi[, 
                                                                     anames], horiz = TRUE, las = 2, main = lsp[i], 
                  beside = TRUE, xlab = "")
        }
      }
    }
    CV.dfs
  }


#*****************************************************************
#8.Tables-Script 5 to have biomass at age for different species-----

age.at.length.importSP_TR=
  
  function (path1, path2, sp = c("SPRA-SPR", "TRAC-TRA"), cruise, 
            plotit = TRUE) 
  {
    if (class(path1) == "data.frame") {
      LA.SP = path1
    }
    else {
      LA.SP = read.csv2(path1)
    }
    names(LA.SP) = c("ref", "date", "NOCHAL", "NOSTA", "Lmm", 
                     "poids", "sexe", "maturite", "age")
    LA.SP$sp =  sp[1]
    LA.SP$CAMPAGNE = cruise
    if (class(path2) == "data.frame") {
      LA.TR = path2
    }
    else {
      LA.TR = read.csv2(path2)
    }
    names(LA.TR) = c("ref", "date", "NOCHAL", "NOSTA", "Lmm", 
                     "poids", "sexe", "maturite", "age")
    LA.TR$sp = sp[2]
    LA.TR$CAMPAGNE = cruise
    head(LA.SP)
    head(LA.TR)
    LA.SPTR = rbind(LA.SP, LA.TR)
    dim(LA.SPTR)
    dim(LA.SPTR)
    LA.SPTRs = LA.SPTR[LA.SPTR$age != -1 & LA.SPTR$age != "", 
                       ]
    dim(LA.SPTRs)
    table(LA.SP$age)
    table(LA.TR$age)
    LA.SPs = LA.SPTRs[LA.SPTRs$sp == "SPRA-SPR", ]
    LA.TRs = LA.SPTRs[LA.SPTRs$sp == "TRAC-TRA", ]
    Af.SP = table(LA.SPs$L, LA.SPs$age)/apply(table(LA.SPs$L, 
                                                    LA.SPs$age), 1, sum)
    Af.TR = table(LA.TRs$L, LA.TRs$age)/apply(table(LA.TRs$L, 
                                                    LA.TRs$age), 1, sum)
    Af.SP.df = data.frame(CAMPAGNE = cruise, GENRE_ESP = sp[1], 
                          TAILLE = as.numeric(rep(dimnames(Af.SP)[[1]], dim(Af.SP)[2])), 
                          AGE = as.numeric(rep(dimnames(Af.SP)[[2]], each = dim(Af.SP)[1])), 
                          POURCENTAGE = c(Af.SP))
    Af.TR.df = data.frame(CAMPAGNE = cruise, GENRE_ESP = sp[2], 
                          TAILLE = as.numeric(rep(dimnames(Af.TR)[[1]], dim(Af.TR)[2])), 
                          AGE = as.numeric(rep(dimnames(Af.TR)[[2]], each = dim(Af.TR)[1])), 
                          POURCENTAGE = c(Af.TR))
    if (plotit) {
      par(mfrow = c(1, 1))
      plot(Af.SP.df[, "TAILLE"], Af.SP.df[, "AGE"], type = "n", 
           xlab = "Length(cm)", ylab = "Age(year)", main = "Sprat", 
           pch = 16, col = 1)
      points(Af.SP.df[Af.SP.df$POURCENTAGE > 0, "TAILLE"], 
             Af.SP.df[Af.SP.df$POURCENTAGE > 0, "AGE"], cex = 1 + 
               Af.SP.df[Af.SP.df$POURCENTAGE > 0, "POURCENTAGE"], 
             pch = 16, col = 1)
      legend("topleft", legend = "% at age", pch = 16)
      plot(Af.TR.df[, "TAILLE"], Af.TR.df[, "AGE"], type = "n", 
           xlab = "Length(cm)", ylab = "Age(year)", main = "Horse mackerel", 
           pch = 16, col = 1)
      points(Af.TR.df[Af.TR.df$POURCENTAGE > 0, "TAILLE"], 
             Af.TR.df[Af.TR.df$POURCENTAGE > 0, "AGE"], cex = 1 + 
               Af.TR.df[Af.TR.df$POURCENTAGE > 0, "POURCENTAGE"], 
             pch = 16, col = 1)
      legend("topleft", legend = "% at age", pch = 16)
    }
    Af.SP.df$TAILLE = as.numeric(as.character(Af.SP.df$TAILLE))
    usize1 = unique(Af.SP.df$TAILLE)
    inc1 = median(usize1[2:length(usize1)] - usize1[1:(length(usize1) - 
                                                         1)])
    Lage.SP = seq(min(Af.SP.df$TAILLE), max(Af.SP.df$TAILLE), 
                  inc1)
    Lage.SP.check = Lage.SP %in% unique(Af.SP.df$TAILLE)
    if (FALSE %in% Lage.SP.check) {
      cat("Missing length class in sprat age at length", 
          Lage.SP[!Lage.SP.check], "\n")
    }
    else {
      cat("No missing length class in sprat age at length", 
          "\n")
    }
    Af.TR.df$TAILLE = as.numeric(as.character(Af.TR.df$TAILLE))
    usize2 = unique(Af.TR.df$TAILLE)
    inc2 = median(usize2[2:length(usize2)] - usize2[1:(length(usize2) - 
                                                         1)])
    Lage.TR = seq(min(Af.TR.df$TAILLE), max(Af.TR.df$TAILLE), 
                  inc2)
    Lage.TR.check = Lage.TR %in% unique(Af.TR.df$TAILLE)
    if (FALSE %in% Lage.TR.check) {
      cat("Missing length class in horse mackerel age at length", 
          Lage.TR[!Lage.TR.check], "\n")
    }
    else {
      cat("No missing length class in horse mackerel age at length", 
          "\n")
    }
    if (FALSE %in% Lage.SP.check) {
      i = 1
      while (sum(Lage.SP.check == FALSE) > 0) {
        posSPcheck = match(FALSE, Lage.SP.check)
        Af.SParound = Af.SP.df[Af.SP.df$TAILLE %in% c(Lage.SP[posSPcheck] - 
                                                        inc1, Lage.SP[posSPcheck] + inc1), ]
        Af.SParounda = aggregate(Af.SParound$POURCENTAGE, 
                                 list(CAMPAGNE = Af.SParound$CAMPAGNE, GENRE_ESP = Af.SParound$GENRE_ESP, 
                                      AGE = Af.SParound$AGE), mean)
        names(Af.SParounda)[4] = "POURCENTAGE"
        Af.SParounda$TAILLE = Lage.SP[posSPcheck]
        SPa = Af.SParounda[, names(Af.SP.df)]
        if (i == 1) {
          Af.SP.df.cor = rbind(Af.SP.df, SPa)
        }
        else {
          Af.SP.df.cor = rbind(Af.SP.df.cor, SPa)
        }
        Lage.SP.check[posSPcheck] = TRUE
        i = i + 1
      }
      cat("Missing length class in sprat age at length added", 
          "\n")
    }
    else {
      Af.SP.df.cor = Af.SP.df
    }
    if (FALSE %in% Lage.TR.check) {
      i = 1
      while (sum(Lage.TR.check == FALSE) > 0) {
        posTRcheck = match(FALSE, Lage.TR.check)
        Af.TRaround = Af.TR.df[Af.TR.df$TAILLE %in% c(Lage.TR[posTRcheck] - 
                                                        inc2, Lage.TR[posTRcheck] + inc2), ]
        Af.TRarounda = aggregate(Af.TRaround$POURCENTAGE, 
                                 list(CAMPAGNE = Af.TRaround$CAMPAGNE, GENRE_ESP = Af.TRaround$GENRE_ESP, 
                                      AGE = Af.TRaround$AGE), mean)
        names(Af.TRarounda)[4] = "POURCENTAGE"
        Af.TRarounda$TAILLE = Lage.TR[posTRcheck]
        TRa = Af.TRarounda[, names(Af.TR.df)]
        if (i == 1) {
          Af.TR.df.cor = rbind(Af.TR.df, TRa)
        }
        else {
          Af.TR.df.cor = rbind(Af.TR.df.cor, TRa)
        }
        Lage.TR.check[posTRcheck] = TRUE
        i = i + 1
      }
      cat("Missing length class in horse mackerel age at length added", 
          "\n")
    }
    else {
      Af.TR.df.cor = Af.TR.df
    }
    LA = rbind(Af.SP.df.cor, Af.TR.df.cor)
    head(LA)
    LAa = aggregate(LA$POURCENTAGE, list(LA$GENRE_ESP, LA$AGE), 
                    sum)
    names(LAa) = c("GENRE_ESP", "AGE", "Pi")
    LA = merge(LA, LAa, by.x = c("GENRE_ESP", "AGE"), by.y = c("GENRE_ESP", 
                                                               "AGE"))
    LA$Ti = LA$TAILLE * LA$POURCENTAGE/LA$Pi
    wmLA = aggregate(LA$Ti, list(LA$GENRE_ESP, LA$AGE), sum)
    names(wmLA) = c("GENRE_ESP", "AGE", "wmL")
    aLA = aggregate(LA$POURCENTAGE, list(LA$GENRE_ESP, LA$TAILLE), 
                    sum)
    aLAs = aLA[(aLA[, 1] == "SPRA-SPR" & !aLA[, 2] %in% Lage.SP[!Lage.SP.check]) & 
                 (aLA[, 1] == "TRAC-TRA" & !aLA[, 2] %in% Lage.TR[!Lage.TR.check]), 
               ]
    aLAb = aLA[aLA[, 3] == 0, ]
    if (sum(aLA[, 3]) != dim(aLA)[1]) {
      stop("Percentage at age do not sum to unity")
    }
    list(LA.SPTRs = LA.SPTRs, Af.SP.df = Af.SP.df, Af.TR.df = Af.TR.df, 
         Af.SP.df.cor = Af.SP.df.cor, Af.TR.df.cor = Af.TR.df.cor, 
         LA = LA)
  }




#*****************************************************************
#9.Graphs-script 5 changed to have biomass at age for different species-----


B.esdu.age.SPTR.plot=
  
  function (path.res.charef = NULL, Biomres.SP.age, Biomres.TR.age) 
  {
    if (!is.null(path.res.charef)) {
      aSP = Biomres.SP.age[, !names(Biomres.SP.age) %in% c("esdu.id", 
                                                           "TC", "LONG", "LAT", "sp", "Ntot")]
      names(aSP)
      Na = dim(aSP)[2]
      for (i in 1:Na) {
        nfile = paste("SPRA-SPR_biomassMapAge", i, "~expert.png", 
                      sep = "")
        png(file = paste(path.res.charef, nfile, sep = ""), 
            width = 600, height = 600)
        plot(Biomres.SP.age$LONG, Biomres.SP.age$LAT, asp = 1/cos(46 * 
                                                                    pi/180), cex = 0.001, xlab = "", ylab = "", main = paste("SPRA-SPR", names(aSP)[i], "abundance per EDSU (No. of fish)"), 
             pch = 16, col = "grey70")
        points(Biomres.SP.age$LONG, Biomres.SP.age$LAT, asp = 1/cos(46 * 
                                                                      pi/180), cex = log(aSP[, i] + 1)/10, xlab = "", 
               ylab = "",pch = 16, col = 1)
        coast()
        dev.off()
      }
      Na = dim(Biomres.TR.age[, !names(Biomres.TR.age) %in% 
                                c("esdu.id", "TC", "LONG", "LAT", "sp", "Ntot")])[2]
      for (i in 1:Na) {
        nfile = paste("TRAC-TRA_biomassMapAge", i, "~expert.png", 
                      sep = "")
        png(file = paste(path.res.charef, nfile, sep = ""), 
            width = 600, height = 600)
        plot(Biomres.TR.age$LONG, Biomres.TR.age$LAT, asp = 1/cos(46 * 
                                                                    pi/180), cex = 0.001, xlab = "", ylab = "", main = paste("TRAC-TRA", 
                                                                                                                             substr(names(Biomres.TR.age)[5 + i], 3, 7), "abundance per EDSU (No. of fish)"), 
             pch = 16, col = "grey70")
        points(Biomres.TR.age$LONG, Biomres.TR.age$LAT, asp = 1/cos(46 * 
                                                                      pi/180), cex = log(Biomres.TR.age[, 5 + i] + 
                                                                                           1)/10, xlab = "", ylab = "",pch = 16, col = 7)
        coast()
        dev.off()
      }
    }
    aSP = Biomres.SP.age[, !names(Biomres.SP.age) %in% c("esdu.id", 
                                                         "TC", "LONG", "LAT", "sp", "Ntot")]
    names(aSP)
    Na = dim(aSP)[2]
    for (i in 1:Na) {
      if (i %in% c(1, 5, 9, 13, 17)) {
        x11()
        par(mfrow = c(2, 2))
      }
      plot(Biomres.SP.age$LONG, Biomres.SP.age$LAT, asp = 1/cos(46 * 
                                                                  pi/180), cex = 0.001, xlab = "", ylab = "", main = paste("SPRA-SPR",names(aSP)[i], "abundance per EDSU (No. of fish)"), 
           pch = 16, col = "grey70")
      points(Biomres.SP.age$LONG, Biomres.SP.age$LAT, asp = 1/cos(46 * 
                                                                    pi/180), cex = log(aSP[, i] + 1)/10, xlab = "", ylab = "",pch = 16, col = 1)
      coast()
    }
    Na = dim(Biomres.TR.age[, !names(Biomres.TR.age) %in% c("esdu.id", 
                                                            "TC", "LONG", "LAT", "sp", "Ntot")])[2]
    for (i in 1:Na) {
      if (i %in% c(1, 5, 9, 13, 17)) {
        x11()
        par(mfrow = c(2, 2))
      }
      plot(Biomres.TR.age$LONG, Biomres.TR.age$LAT, asp = 1/cos(46 * 
                                                                  pi/180), cex = 0.001, xlab = "", ylab = "", main = paste("TRAC-TRA", 
                                                                                                                           substr(names(Biomres.TR.age)[5 + i], 3, 7), "abundance per EDSU (No. of fish)"), 
           pch = 16, col = "grey70")
      points(Biomres.TR.age$LONG, Biomres.TR.age$LAT, asp = 1/cos(46 * 
                                                                    pi/180), cex = log(Biomres.TR.age[, 5 + i] + 1)/10, 
             xlab = "", ylab = "",pch = 16, col = 7)
      coast()
    }
  }













#*********************************************************************************************************************
