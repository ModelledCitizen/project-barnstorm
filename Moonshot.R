library(rgdal)
library(rgeos)
library(leaflet)
library(purrr)

PA.prec <- readOGR("Geo/Penn/Precincts.shp")
phil.ward <- readOGR("Geo/Penn/Phil/Wards.geojson")
phil.divs <- readOGR("Geo/Penn/Phil/Divisions.geojson")

rgx <- "as.factor\\(vb__vf_county_code\\)([[:digit:]]+)"
load("Data/geo- PA -test-precincts-07022017.Rdata")
idx <- grep(rgx, rownames(lout.coef2))
PA.cres <-
    data.frame(
        COUNTYFP = sub(rgx, "\\1", rownames(lout.coef2)[idx]),
        estimate = lout.coef2[idx, 1],
        std.est = scale(lout.coef2[idx, 1]),
        std.error = lout.coef2[idx, 2],
        t.value = lout.coef2[idx, 3],
        prob.t = lout.coef2[idx, 4],
        row.names = NULL,
        stringsAsFactors = F
    )
PA.pres <-
    data.frame(
        COUNTYFP = sout2$vb__vf_county_code,
        PRECNAME = toupper(sout2$vb__vf_precinct_name),
        resid.mean = sout2$residuals.mean,
        resid.mean2 = scale(sout2$residuals.mean),
        row.names = NULL,
        stringsAsFactors = F
    )
PA.pres <-
    merge(PA.pres,
          PA.cres[, 1:2],
          by = "COUNTYFP",
          all.x = T,
          sort = F)
PA.pres$resid.mean3 <- PA.pres$resid.mean + PA.pres$estimate + lout.coef2[1, 1]
PA.pres$resid.mean4 <- scale(PA.pres$resid.mean3)
PA.pres$estimate <- NULL

phil.pres <- PA.pres[PA.pres$COUNTYFP == "101", ]
rm(idx, lout.coef2, sout2, tout2, rgx)

phil.pres$MATCHID <-
    sub("PHILADELPHIA ([[:digit:]]{2})-([[:digit:]]{2})",
        "\\1\\2",
        phil.pres$PRECNAME)

phil.divs$MATCHID <- phil.divs$DIVISION_NUM

phil.divs$ORDER <- 1:nrow(phil.divs)

phil.divs <-
    merge(phil.divs,
          phil.pres,
          by = "MATCHID",
          all.x = T,
          sort = F)

phil.divs <- phil.divs[order(phil.divs$ORDER),]

phil.divs$cats <-
    cut(phil.divs$resid.mean4, c(-Inf, -2, -1, 0, 1, 2, Inf), c(1:6))

phil.divs$cats <- (1:6)[phil.divs$cats]

phil.addr <- readOGR("Geo/tl_2016_42101_addrfeat.shp")
phil.addr <- spTransform(phil.addr, CRSobj = CRS(proj4string(phil.divs)))

phil.nbrs <- gTouches(phil.divs, byid = TRUE, returnDense = FALSE)

phil.nbr1 <-
    lapply(seq_along(phil.nbrs), function(x)
        data.frame(
            OID = x,
            OWR = substr(phil.divs$DIVISION_NUM[x], 1, 2),
            ODI = substr(phil.divs$DIVISION_NUM[x], 3, 4),
            OCT = phil.divs$cats[x],
            NID = phil.nbrs[[x]],
            NWR = substr(phil.divs$DIVISION_NUM[phil.nbrs[[x]]], 1, 2),
            NDI = substr(phil.divs$DIVISION_NUM[phil.nbrs[[x]]], 3, 4),
            NCT = phil.divs$cats[phil.nbrs[[x]]],
            DIF = abs(phil.divs$cats[x] - phil.divs$cats[phil.nbrs[[x]]])
        ))

phil.nbr2 <- reduce(phil.nbr1, rbind)

phil.nbr3 <- phil.nbr2
phil.nbr3$AID <- NA
centroids <- gCentroid(phil.addr, byid = T)
for (r in 1:nrow(phil.nbr2)) {
    if (phil.nbr2$DIF[r] %in% 2:4) {
        border <-
            gIntersection(phil.divs[phil.nbr2$OID[r],], phil.divs[phil.nbr2$NID[r],])
        if (class(border) == "SpatialCollections") {
            border <- border@lineobj
        }
        centroidb <- gCentroid(border)
        closest <- gDistance(centroidb, centroids, byid = T)
        street <- phil.addr[order(closest)[1:2],]
        str <- gIntersection(border, street)
        if (!is.null(str)) {
            if (gLength(str) == 0) {
                unique(sort(closest))
            street <- phil.addr[order(closest)[3:4],]
            }
        }
        plot(street)
        plot(border, add = T, col = "red")
        browser()
    }
}

# if (class(border) == "SpatialCollections") {
#     border <- border@lineobj
# }
# str <- unlist(gIntersects(
#     border,
#     phil.addr,
#     byid = T,
#     returnDense = F
# ))
# tr <- c()
# for (i in str) {
#     st <- gIntersection(border, phil.addr[i, ])
#     tf <- class(st) != "SpatialPoints"
#     tr <- c(tr, tf)
# }
# if (sum(tr) != 0) {
#     phil.nbr3$AID[r] <- str[tr]
# }

for (r in 1:nrow(phil.nbr2)) {
    if (phil.nbr2$DIF[r] %in% 2:4) {
        border <-
            gIntersection(phil.divs[phil.nbr2$OID[r], ], phil.divs[phil.nbr2$NID[r], ])
        plot(border)
        str <- unlist(gIntersects(
            border,
            phil.addr,
            byid = T,
            returnDense = F
        ))
        plot(phil.addr[str,], add = T, col = "red")
        browser()
    }
}

plot(phil.addr)
for (r in 1:nrow(phil.nbr2)) {
    if (phil.nbr2$DIF[r] %in% 2:4) {
        border <-
            gIntersection(phil.divs[phil.nbr2$OID[r],], phil.divs[phil.nbr2$NID[r],])
        if (class(border) != "SpatialPoints") {
            plot(border, add = T, col = "red")
        }
    }
}

strpal <- colorBin(
    "Greys",
    phil.divs$resid.mean4,
    na.color = "white",
    bins = c(-Inf, -2, -1, 0, 1, 2, Inf)
)
pal2 <- colorBin(
    "Greys",
    phil.divs$resid.mean2,
    na.color = "white",
    bins = c(-Inf, -2, -1, 0, 1, 2, Inf)
)

lab <-
    paste0(
        "<strong>",
        phil.divs$PRECNAME,
        "</strong><br>Std. Resid. Mean F.E.: ",
        round(phil.divs$resid.mean4, 4),
        "<br>Std. Resid. Mean: ",
        round(phil.divs$resid.mean2, 4),
        "<br>Resid. Mean F.E.: ",
        round(phil.divs$resid.mean3, 4),
        "<br>Resid. Mean: ",
        round(phil.divs$resid.mean, 4)
    ) %>% lapply(htmltools::HTML)

leaflet(phil.divs) %>%
    addProviderTiles("CartoDB", group = "CartoDB") %>%
    addPolygons(
        fillColor = ~ pal(resid.mean4),
        weight = 1,
        opacity = 1,
        color = "#000000",
        fillOpacity = 1,
        smoothFactor = 0.5,
        highlight = highlightOptions(
            weight = 3,
            color = "white",
            fillOpacity = 0,
            bringToFront = T,
            sendToBack = T
        ),
        label = lab,
        labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"
        ),
        group = "Fixed Effects"
    ) %>%
    addPolygons(
        fillColor = ~ pal2(resid.mean2),
        weight = 1,
        opacity = 1,
        color = "#000000",
        fillOpacity = 1,
        smoothFactor = 0.5,
        highlight = highlightOptions(
            weight = 3,
            color = "white",
            fillOpacity = 0,
            bringToFront = T,
            sendToBack = T
        ),
        label = lab,
        labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"
        ),
        group = "No Fixed Effects"
    ) %>%
    addPolylines(
        data = phil.ward,
        fill = F,
        weight = 1,
        color = "red",
        opacity = "1",
        smoothFactor = 0.5,
        group = "Wards"
    ) %>%
    addLegend(
        pal = pal,
        values = ~ resid.mean4,
        opacity = 1,
        title = NULL,
        position = "bottomright",
        group = "Fixed Effects"
    ) %>% hideGroup("No Fixed Effects") %>%
    addLayersControl(
        baseGroups = c("Fixed Effects", "No Fixed Effects"),
        overlayGroups = c("Wards"),
        options = layersControlOptions(collapsed = F)
    )
