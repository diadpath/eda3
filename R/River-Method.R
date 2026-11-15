#' @include River-Class.R River-Methods.R Eda-Class.R Eda-generics.R Eda-utils.R
NULL

#' loaddb method for class Rivers
#'
#' @note Connection information comes from `object@base` (see class `Eda`) and the
#' method uses the river_network defined in the class.
#' @param object an object of class River
#' @param selected_eda_column a vector of eda columns following the eda naming
#' convention (see `column_mapping`) to be used in further modelling when loading
#' one of the river networks
#' default sudoang, possible values hydrosheds
#' @return An object of class River with data loaded.
setMethod(
  "loaddb",
  signature = signature("River"),
  definition = function(object,
                        selected_eda_columns
                        ) {
    river_network <- object@river_network
    con <- .eda_connect(object)
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    select_clause <- .build_select_mapping(
      con,
      selected_eda_columns,
      river_network
    )

    select <- glue::glue_sql(
      "SELECT  {`select_clause`*}",
      .con = con)

    # Build the filters as a list of conditions combined first with WHERE THEN AND

    conditions <- character(0L)
    country <- .eda_country(object@country)
    if (length(country) > 0L) {
      conditions <- c(conditions,
                      as.character(glue::glue_sql("rn.country IN ({country*})", .con = con)))
    }
    if (length(object@emu) > 0L) {
      emu <- object@emu
      conditions <- c(conditions,
                      as.character(glue::glue_sql("emu IN ({emu*})", .con = con)))
    }
    if (length(object@basin) > 0L) {
      basin <- object@basin
      conditions <- c(conditions,
                      as.character(glue::glue_sql("basin IN ({basin*})", .con = con)))
    }
    where <- if (length(conditions)) {
      paste("WHERE", paste(conditions, collapse = " AND "))
    } else {
      ""
    }
    order_by <- paste0("ORDER BY rn.", object@prkey)
    sql <- paste(select, where, order_by, sep = "\n")

    t1 <- Sys.time()
    object@data <- DBI::dbGetQuery(con, sql)
    cat(paste(sql, "\n"))
    cat(paste("time for query (mins)",
              round(difftime(Sys.time(), t1, units = "mins"), 1), "\n"))
    return(object)
  }
)


#' loadshp method for class River
#'
#' Fills `@shp` with the primary key and the geometry of the river segments.
#' Connection information comes from `object@base`.
#' @param object an object of class River.
#' @param geom name of the geometry column default geom.
setMethod(
  "loadshp",
  signature = signature("River"),
  definition = function(object,
                        schema,
                        table,
                        key = "idsegment",
                        geom = "geom") {
    con <- .eda_connect(object)
    on.exit(DBI::dbDisconnect(con), add = TRUE) # single disconnect

    basin  <- object@basin
    country    <- object@country
    area <- object@area
    country   <- .eda_country(object@country)

    select <- glue::glue_sql(
      "SELECT r.{`key`}, {`geom`} FROM {`schema`}.{`table`} r
       JOIN {`schema`}.{`table2`} r1 ON r.{`key`} = r1.{`key`}",
      .con = con
    )
    where <- if (length(country) > 0L) {
      glue::glue_sql(" WHERE country IN ({country*})", .con = con)
    } else {
      " WHERE TRUE"
    }
    and1 <- if (length(area) > 0L) {
      glue::glue_sql(" AND area IN ({area*})", .con = con)
    } else {
      " AND TRUE"
    }
    query <- paste0(select, where, and1)
    object@shp <- sf::st_read(dsn = con, query = query)
    return(object)
  }
)

#' Method to calculate the distances from the sea
#' @return River
#' pour debug object<-rios_dist
#' troncons_bordure<-217937 # Spain (4452)
#' troncons_bordure<-100072 # Portugal
#' riosdata$downs_point <- ifelse(riosdata$dmer == 0, riosdata$target,  0)
#' riosdata$downs_point[is.na(riosdata$downs_point)] <- 0
#' @author Cedric Briand \email{cedric.briand@@eaux-et-vilaine.bzh}

setMethod(
  "distance_sea",
  signature = signature("River"),
  definition = function(object, datafrontier = NULL) {
    # colonnes utiles
    # gid
    # nextdowid l'gid du tronon aval
    # length(m)
    if (!is.null(datafrontier)) {
      if (!all(c("distanceseam", "idsegment") %in% colnames(datafrontier))) {
        stop("colnames distanceseam, and idsegment should be in the data frame datafrontier")
      }
    }
    riosdata <-
      object@data # plus facile pour la dbug d'avoir le nom utilis dans le prg de test

    if (is.null(datafrontier)) {
      troncons_bordure <- riosdata[riosdata$isfrontier, "idsegment"]
    } else {
      troncons_bordure <- datafrontier$idsegment
    }

    # fonction qui renvoit l'index pour extraire de la table riosdata les donnes correspondant aux tronons tr pour une colonne donne
    # tr peut tre rpt mais chaque instance n'a qu'une ligne (cl primaire) dans la table
    mtch <- function(tr, col) {
      return(match(tr, riosdata[, col])[!is.na(match(tr, riosdata[, col]))])
    }
    # fonction qui renvoit logique pour extraire de la table riosdata les donnes correspondant aux tronons tr pour une colonne donne
    # tr ne peut pas avoir de rptitions mais peut correspondre  plusieurs lignes dans riosdata
    drain_in_tr <- function(tr, col) {
      return(riosdata[, col] %in% tr)
    }
    # Creation d'un barre de progression
    progres <- winProgressBar(
      title = "progression parcours Rios",
      label = paste("progression",
                    min = 0,
                    max = 1
      ),
      initial = 0,
      width = 400
    )
    troncon_mer <- riosdata[as.logical(riosdata$issea), "idsegment"]
    cur_seg <- c(troncons_bordure, troncon_mer)
    traveled_segments <- c(troncons_bordure, troncon_mer)
    riosdata$distanceseam <- NA
    riosdata$path <- NA
    # browser()
    # first step in the routine uses sea segments and frontier
    riosdata[mtch(troncons_bordure, "idsegment"), "path"] <-
      riosdata[mtch(troncons_bordure, "idsegment"), "idsegment"]
    if (!is.null(datafrontier)) {
      riosdata[mtch(troncons_bordure, "idsegment"), "distanceseam"] <-
        datafrontier$distanceseam
    }
    # it's the same in the ccm the closest ccm segment includes its own distance
    #+ 			riosdata[mtch(troncons_bordure,"idsegment"),"lengthm"]

    riosdata[mtch(troncon_mer, "idsegment"), "distanceseam"] <-
      riosdata[mtch(troncon_mer, "idsegment"), "lengthm"]
    riosdata[mtch(troncon_mer, "idsegment"), "path"] <-
      riosdata[mtch(troncon_mer, "idsegment"), "idsegment"]

    step <- 0
    while (length(cur_seg) != 0) {
      step <- step + 1
      # set progress bar
      setWinProgressBar(
        progres,
        length(traveled_segments) / nrow(riosdata),
        label = paste(
          "lev=",
          step,
          "sgcur",
          length(cur_seg),
          "segcum",
          length(traveled_segments),
          "/",
          nrow(riosdata)
        )
      )
      # Segments immediately upstream from the current segment.
      upst_seg <-
        riosdata[drain_in_tr(cur_seg, "nextdownidsegment"), "idsegment"]
      # segments immediately downstream from the upstream segments
      # (this is not exactly the same as the previous step
      # as some are not continued upstream...
      # downst_seg can have repeated values....
      downst_seg <-
        riosdata[mtch(upst_seg, "idsegment"), "nextdownidsegment"]
      stopifnot(length(downst_seg) == length(upst_seg))
      # creation des ltree
      riosdata[mtch(upst_seg, "idsegment"), "path"] <-
        str_c(
          riosdata[mtch(downst_seg, "idsegment"), "path"],
          ".",
          riosdata[mtch(upst_seg, "idsegment"), "idsegment"]
        )
      # remplissage des distances

      riosdata[mtch(upst_seg, "idsegment"), "distanceseam"] <-
        riosdata[mtch(downst_seg, "idsegment"), "distanceseam"] +
        riosdata[mtch(upst_seg, "idsegment"), "lengthm"]
      # to avoid going two times in the same place
      # saving the id of traveled segments
      traveled_segments <- unique(c(traveled_segments, upst_seg))
      # loop
      cur_seg <- upst_seg
    } # end while


    close(progres)
    # stopifnot(length(unique(traveled_segments))==nrow(riosdata))
    object@data <- riosdata
    object@traveled_segments <- traveled_segments
    return(object)
  }
)


#' Method to calculate the Shreve order and strahler cumulated water surface
#' @note the function will select upstream segments and process from the source to the sea
#' the column to fill in are called strahler, shreeve and surfacebvm2
#' the surface of the unit bv is stored in surfavebvunitm2
#' @param silent= if not the query appears
#' @param from the first line
#' @param to the last line
#' @param vect a vector of gids (default to null)
#' @return River
#' pour debug
#' object<-rios_surf_sub ; silent<-FALSE;from=1;to=nrow(object@data)
#' object<-rios_surf ; silent<-TRUE;from=1;to=nrow(object@data); checksegment=NULL
#'  checksegment<-'PT11784'
setMethod(
  "stream_order",
  signature = signature("River"),
  definition = function(object,
                        silent = TRUE,
                        from = 1,
                        to = nrow(object@data),
                        checksegment = NULL) {
    progres <- winProgressBar(
      title = "progression noeuds",
      label = paste("progression",
                    min = 0,
                    max = 1
      ),
      initial = 0,
      width = 400
    )

    sumstrahler <- function(x) {
      maxstrahler <- max(x, na.rm = TRUE)
      # if there are two number at the max rank strahler change order
      strahler <- maxstrahler
      if (sum(x == maxstrahler) > 1) {
        strahler <- maxstrahler + 1
      }
      return(strahler)
    } # sumstrahler(c(1,1));sumstrahler(c(1,3)); sumstrahler(1)

    # function for two vectors of the same length

    # function which returns the index of active segments in the dataset
    # argument keep the index removing all segments with problems (sea, frontier or NA)
    # returns an index of how to choose the segments on the whole dataset
    fn_idx_active <- function(mtchup, keep, checksegment) {
      # segments upstream from checksegment
      # first filter downstream segments
      downstream_segments <-
        rios$nextdownidsegment[mtchup][keep] # mtchup=boolean indicating active segments; keep= boolean indicating if some segments(sea, frontier or NA) should not be included
      uk_downstream_segments <-
        unique(downstream_segments) # unique downstream segments
      upstream_segments_all <-
        rios$idsegment[rios$nextdownidsegment %in% uk_downstream_segments] # active segments
      downstream_segments_all <-
        rios$nextdownidsegment[rios$nextdownidsegment %in% uk_downstream_segments] # downstream segments both for current active segments and other potential segments
      count_all <-
        data.frame(
          "Ntot" = as.numeric(
            tapply(upstream_segments_all, downstream_segments_all, length)
          ),
          "nextdownidsegment" = names(
            tapply(upstream_segments_all, downstream_segments_all, length)
          )
        ) # count on the total number of upstream segments to a given downstream segment
      count_current <-
        data.frame(
          "Ncurrent" = as.numeric(tapply(
            rios$idsegment[mtchup][keep], downstream_segments, length
          )),
          "nextdownidsegment" = names(tapply(
            rios$idsegment[mtchup][keep], downstream_segments, length
          ))
        ) # count on the number of upstream segments for the active paths
      count <- merge(count_all, count_current)
      if (!is.null(checksegment)) {
        if (checksegment %in% count$nextdownidsegment) {
          cat(paste(count[count$nextdownidsegment == checksegment, ], collapse = "||", "\n"))
        }
      }
      # we only keep the segments that are actually run
      if (nrow(count) > 0) {
        downstream_segments_active <-
          count$nextdownidsegment[count$Ntot == count$Ncurrent] # idsegments where those counts are equal
      } else {
        return(rep(FALSE, nrow(rios)))
      }
      activeidx <-
        rios$nextdownidsegment %in% downstream_segments_active # boolean indicating which segments to check next!
      return(activeidx)
    }

    rios <- object@data
    rios <-
      rios[order(rios$idsegment), ] # As I'm adding later some values from tapply, this is very important
    traveled_segments <- vector()
    upstream_segments <-
      rios$idsegment[(as.logical(rios$issource) &
                        !as.logical(rios$isendoreic))]
    seaidsegments <- rios$idsegment[as.logical(rios$issea)]
    frontieridsegments <-
      rios$idsegment[as.logical(rios$isfrontier) &
                       !is.na(rios$isfrontier)]
    frontieridsegmentsdownstream <-
      rios$nextdownidsegment[as.logical(rios$isfrontier)]
    # need also to remove those segments that will be called as nextdownidsegment
    frontieridsegmentsdownstream <-
      frontieridsegmentsdownstream[!is.na(frontieridsegmentsdownstream)]
    mtchup <- rios$idsegment %in% upstream_segments # segments at source


    # here I'm running the international script
    # so I don't have values from stream param such as surface... coming from Italy or swizerland... I'm ignoring those for the moment
    rios$shreeve <- 0
    # rios$shreeve[!as.logical(rios$isfrontier)]<-0 # rank of shreve
    rios$shreeve[mtchup] <- 1 # fill in the sources as shreeve 1

    rios$strahler <- 0
    # rios$strahler[!as.logical(rios$isfrontier)]<-0 # rank of strahler initiated to zero for most except frontier
    rios$strahler[mtchup] <- 1

    rios$surfacebvm2 <- 0
    # rios$surfacebvm2[!as.logical(rios$isfrontier)]<-0
    rios$surfacebvm2[mtchup] <- rios$surfaceunitbvm2[mtchup]

    rios$distancesourcem <- 0
    # rios$distancesourcem[!as.logical(rios$isfrontier)]<-0
    rios$distancesourcem[mtchup] <- rios$lengthm[mtchup]

    mtchup <- rios$idsegment %in% upstream_segments # | rios$idsegment%in%frontieridsegments
    current_segments <- upstream_segments
    if (!silent) {
      graphics.off()
      X11(
        width = 4,
        height = 4,
        xpos = 0,
        ypos = 0
      )
      nf <- layout(matrix(c(1, 2, 3)))
      # layout.show(nf)
      hist(rios$strahler[current_segments %in% rios$idsegment])
      hist(rios$shreeve[current_segments %in% rios$idsegment])
      hist(log(rios$surfacebvm2[current_segments %in% rios$idsegment]))

      # layout(matrix(1:3),1,3)
      object <-
        loadshp(object) # remplit le @shp avec juste id_segment et geom
      shp <-
        merge(object@shp, rios[, c(
          object@prkey,
          "strahler",
          "shreeve",
          "surfacebvm2",
          "distancesourcem"
        )], by = "idsegment")
      x11(
        width = 4,
        height = 4,
        xpos = 500,
        ypos = 0
      )
      plot(shp[c("strahler")], lwd = 2)
      x11(
        width = 4,
        height = 4,
        xpos = 0,
        ypos = 500
      )
      plot(shp[c("shreeve")], lwd = 2)
      x11(
        width = 4,
        height = 4,
        xpos = 500,
        ypos = 500
      )
      shp$surfacebvm2 <- shp$surfacebvm2 + 0.1
      plot(shp[c("surfacebvm2")], logz = TRUE, lwd = 2)
      x11(
        width = 4,
        height = 4,
        xpos = 500,
        ypos = 500
      )
      plot(shp[c("distancesourcem")], lwd = 2)
      # Sys.sleep(10)
    }

    if (!is.null(checksegment)) {
      theseaidsegment <- rios[rios$idsegment == checksegment, "seaidsegment"]
      graphics.off()
      X11(
        width = 4,
        height = 4,
        xpos = 0,
        ypos = 0
      )
      nf <- layout(matrix(c(1, 2, 3)))
      # layout.show(nf)
      hist(rios$strahler[current_segments %in% rios$idsegment])
      hist(rios$shreeve[current_segments %in% rios$idsegment])
      hist(log(rios$surfacebvm2[current_segments %in% rios$idsegment]))

      # layout(matrix(1:3),1,3)
      object <-
        loadshp(object) # remplit le @shp avec juste id_segment et geom
      shp <-
        merge(object@shp, rios[rios$seaidsegment == theseaidsegment &
                                 !is.na(rios$seaidsegment), c(
                                   object@prkey,
                                   "strahler",
                                   "shreeve",
                                   "surfacebvm2",
                                   "distancesourcem"
                                 )], by = "idsegment")
      x11(
        width = 4,
        height = 4,
        xpos = 500,
        ypos = 0
      )
      plot(shp[c("strahler")])
      x11(
        width = 4,
        height = 4,
        xpos = 0,
        ypos = 500
      )
      plot(shp[c("shreeve")])
      x11(
        width = 4,
        height = 4,
        xpos = 500,
        ypos = 500
      )
      shp$surfacebvm2 <- shp$surfacebvm2 + 0.1
      plot(shp[c("surfacebvm2")], logz = TRUE)
      x11(
        width = 4,
        height = 4,
        xpos = 500,
        ypos = 500
      )
      plot(shp[c("distancesourcem")])
    }
    # initialize variables
    traveled_segments <- current_segments
    i <- 0
    level <- 0
    upstream_segments_left <- upstream_segments
    nb <- length(current_segments)
    countforbreakfinal <- 0
    print("sum of upstream segments")
    while (length(current_segments) > 0) {
      level <- level + 1
      i <- length(traveled_segments)
      setWinProgressBar(
        progres,
        i / length(rios$idsegment),
        label = paste(
          "length current seg=",
          length(current_segments),
          "level=",
          level
        )
      )

      downstream_segments_init <-
        rios$nextdownidsegment[mtchup] # this is now downstream 1 rank from the first segments
      if (!is.null(checksegment)) {
        if (checksegment %in% downstream_segments_init) {
          cat(
            sprintf(
              "segment %s is in downstream segments init at level %s \n",
              checksegment,
              level
            )
          )
        }
      }
      # this is already so when level=1
      upstream_segments <- rios$idsegment[mtchup]
      # test <- 'SP298445'%in%downstream_segments_init | 'PT76749'%in%downstream_segments_init

      # we need to remove NA, frontier, the sea idsegments and the frontier idsegments, here we stop
      nonna <- !is.na(downstream_segments_init)
      # if (level==1){
      # allow frontier segments on the first run
      # nonfrontierdownstream<- rep(TRUE, length(downstream_segments_init))
      # } else {
      # 	nonfrontierdownstream <- !downstream_segments_init%in%frontieridsegmentsdownstream
      # }

      keep <-
        nonna # &  nonfrontierdownstream	 #nonsea & nonfrontier &
      # returns an index calculated based on downstream_segments and indicating which downstreamsegments have all ancerstors
      activeidx <- fn_idx_active(mtchup, keep, checksegment)
      # from now on only working with those ancestors
      # test <- 'SP298445'%in%rios$nextdownidsegment[activeidx]| 'PT76749'%in%rios$nextdownidsegment[activeidx]
      # 	if (test) browser()

      sum_shreeve_rank <-
        tapply(rios$shreeve[activeidx], rios$nextdownidsegment[activeidx], sum,
               na.rm =
                 TRUE
        )
      sum_strahler_rank <-
        tapply(rios$strahler[activeidx], rios$nextdownidsegment[activeidx], sumstrahler)
      sum_surfacebvm2 <-
        tapply(rios$surfacebvm2[activeidx], rios$nextdownidsegment[activeidx], sum,
               na.rm =
                 TRUE
        )
      sum_distancesourcem <-
        tapply(rios$distancesourcem[activeidx], rios$nextdownidsegment[activeidx], max)

      # this is now downstream from the previous step, in first line only the segments with all ancestors are keep
      downstream_segments <-
        rios$nextdownidsegment[activeidx] # this is now downstream 1 rank from the first segments
      uk_downstream_segments <- unique(downstream_segments)
      # downstream_segments_left<-unique(downstream_segments_all[!downstream_segments_all%in%uk_downstream_segments])

      # upstream segments where all ancestor are found downstream
      # the following are using as idsegment is a primary keep
      upstream_segments_done <- rios$idsegment[activeidx]
      # upstream segments without all ancestor at this step
      upstream_segments_left <-
        upstream_segments[!upstream_segments %in% upstream_segments_done]


      # mtchdown <- rios$idsegment%in%uk_downstream_segments	# within the entire vector, which are in 'downstream_segments'


      # To take care of routing problems
      # 				if(!sum(mtchdown)==length(sum_shreeve_rank)){
      # 					lostones <-names(sum_shreeve_rank)[!names(sum_shreeve_rank)%in%rios$idsegment[activeidx]]
      # 					sum_shreeve_rank<- sum_shreeve_rank[!names(sum_shreeve_rank)%in%lostones]
      # 					sum_strahler_rank<- sum_strahler_rank[!names(sum_strahler_rank)%in%lostones]
      # 					sum_surfacebvm2<- sum_surfacebvm2[!names(sum_surfacebvm2)%in%lostones]
      # 					warnings(sprintf("level=%s,removed from stream routing %s", level,paste(lostones,collapse=",")))
      # 				}
      nam <- match(names(sum_shreeve_rank), rios$idsegment)
      # if (any(is.na(nam))) stop("NA names these should have been removed by the function before : check")

      # set sheeve and strahler
      rios$shreeve[nam[!is.na(nam)]] <- sum_shreeve_rank[!is.na(nam)]
      rios$strahler[nam[!is.na(nam)]] <- sum_strahler_rank[!is.na(nam)]
      surfaceunitnam <- rios$surfaceunitbvm2[nam[!is.na(nam)]]
      surfaceunitnam[is.na(surfaceunitnam)] <-
        0 # bug in portugal some surface are zero
      rios$surfacebvm2[nam[!is.na(nam)]] <-
        sum_surfacebvm2[!is.na(nam)] + surfaceunitnam
      lengthnam <- rios$lengthm[nam[!is.na(nam)]]
      lengthnam[is.na(lengthnam)] <- 0
      rios$distancesourcem[nam[!is.na(nam)]] <-
        sum_distancesourcem[!is.na(nam)] + lengthnam

      if (!silent) {
        dev.set(dev.list()[1])
        hist(rios$strahler[current_segments %in% rios$idsegment])
        hist(rios$shreeve[current_segments %in% rios$idsegment])
        hist(log(rios$surfacebvm2[current_segments %in% rios$idsegment]))
        shp <- merge(object@shp,
                     rios[
                       ,
                       c(
                         object@prkey,
                         "strahler",
                         "shreeve",
                         "surfacebvm2",
                         "distancesourcem"
                       )
                     ],
                     by = "idsegment"
        )
        dev.set(dev.next())
        plot(shp[c("strahler")],
             main = str_c("strahler, level ", level),
             lwd = 2
        )
        dev.set(dev.next())
        plot(shp[c("shreeve")],
             main = str_c("shreeve , level ", level),
             lwd = 2
        )
        dev.set(dev.next())
        shp$surfacebvm2 <- shp$surfacebvm2 / 10^6 + 0.1
        plot(
          shp[c("surfacebvm2")],
          logz = TRUE,
          main = str_c("Surface km2 log scale, level ", level),
          lwd = 2
        )
        dev.set(dev.next())
        plot(shp[c("distancesourcem")],
             main = str_c("distance source (m) , level ", level),
             lwd = 2
        )
        # Sys.sleep(2)
      }
      if (!is.null(checksegment)) {
        theupstreamsegments <-
          rios[rios$nextdownidsegment == checksegment &
                 !is.na(rios$nextdownidsegment), "idsegment"]
        # this will display at previous step
        if (any(uk_downstream_segments %in% theupstreamsegments)) {
          theseaidsegment <- rios[rios$idsegment == checksegment, "seaidsegment"]
          shp <- merge(object@shp,
                       rios[
                         rios$seaidsegment == theseaidsegment &
                           !is.na(rios$seaidsegment),
                         c(
                           object@prkey,
                           "strahler",
                           "shreeve",
                           "surfacebvm2",
                           "distancesourcem"
                         )
                       ],
                       by = "idsegment"
          )
          x11()
          plot(shp[c("strahler")], main = str_c("strahler, level ", level))
          x11()
          plot(shp[c("shreeve")], main = str_c("shreeve , level ", level))
          x11()
          shp$surfacebvm2 <- shp$surfacebvm2 / 10^6 + 0.1
          plot(shp[c("surfacebvm2")],
               logz = TRUE,
               main = str_c("Surface km2 log scale, level ", level)
          )
          x11()
          plot(shp[c("distancesourcem")], main = str_c("distance source (m) , level ", level))
          # browser()
        }
      }
      traveled_segments <-
        unique(c(traveled_segments, downstream_segments)) # to change the progress bar in the next loop
      old_current_segments <- current_segments # used for break below
      current_segments <-
        unique(c(uk_downstream_segments, upstream_segments_left))
      nonsea <- !current_segments %in% seaidsegments
      # nonfrontier <- !current_segments%in%frontieridsegments
      current_segments <- current_segments[nonsea]
      mtchup <-
        rios$idsegment %in% current_segments # changing the vector of match to the new level
      oldnb <- nb
      nb <- sum(mtchup)
      if (oldnb == nb &
          identical(old_current_segments, current_segments)) {
        countforbreakfinal <- countforbreakfinal + 1
      }
      if (countforbreakfinal == 2) {
        break()
      }
      print(nb)
    }
    close(progres)
    if (!is.null(checksegment)) {
      theseaidsegment <- rios[rios$idsegment == checksegment, "seaidsegment"]
      shp <- merge(object@shp,
                   rios[
                     rios$seaidsegment == theseaidsegment &
                       !is.na(rios$seaidsegment),
                     c(
                       object@prkey,
                       "strahler",
                       "shreeve",
                       "surfacebvm2",
                       "distancesourcem"
                     )
                   ],
                   by = "idsegment"
      )
      x11()
      plot(shp[c("strahler")], main = str_c("strahler, level ", level))
      x11()
      plot(shp[c("shreeve")], main = str_c("shreeve , level ", level))
      x11()
      shp$surfacebvm2 <- shp$surfacebvm2 / 10^6 + 0.1
      plot(shp[c("surfacebvm2")],
           logz = TRUE,
           main = str_c("Surface km2 log scale, level ", level)
      )
      x11()
      plot(shp[c("distancesourcem")], main = str_c("distance source (m) , level ", level))
    }


    object@data <- rios
    return(object)
  }
)


#' Method to calculate the silver eel production, adapted from stream_order
#' @note the function will select upstream segments and process from the source to the sea
#' the column to fill
#' @return River
#' pour debug
#' object<-rios_surf
setGeneric(
  "silver_eel_production",
  def = function(object, ...) {
    standardGeneric("silver_eel_production")
  }
)
setMethod(
  "silver_eel_production",
  signature = signature("River"),
  definition = function(object, checksegment = NULL) {
    progres <- winProgressBar(
      title = "progression noeuds",
      label = paste("progression",
                    min = 0,
                    max = 1
      ),
      initial = 0,
      width = 400
    )


    # function for two vectors of the same length

    # function which returns the index of active segments in the dataset
    # argument keep the index removing all segments with problems (sea, frontier or NA)
    # returns an index of how to choose the segments on the whole dataset
    fn_idx_active <- function(mtchup, keep, checksegment) {
      # segments upstream from checksegment
      # first filter downstream segments
      downstream_segments <- rios$nextdownidsegment[mtchup][keep]
      uk_downstream_segments <- unique(downstream_segments)
      upstream_segments_all <-
        rios$idsegment[rios$nextdownidsegment %in% uk_downstream_segments] # 72
      downstream_segments_all <-
        rios$nextdownidsegment[rios$nextdownidsegment %in% uk_downstream_segments]
      count_all <-
        data.frame(
          "Ntot" = as.numeric(
            tapply(upstream_segments_all, downstream_segments_all, length)
          ),
          "nextdownidsegment" = names(
            tapply(upstream_segments_all, downstream_segments_all, length)
          )
        )
      count_current <-
        data.frame(
          "Ncurrent" = as.numeric(tapply(
            rios$idsegment[mtchup][keep], downstream_segments, length
          )),
          "nextdownidsegment" = names(tapply(
            rios$idsegment[mtchup][keep], downstream_segments, length
          ))
        )
      count <- merge(count_all, count_current)
      if (!is.null(checksegment)) {
        if (checksegment %in% count$nextdownidsegment) {
          cat(paste(count[count$nextdownidsegment == checksegment, ], collapse = "||", "\n"))
        }
      }
      # we only keep the segments that are actually run
      if (nrow(count) > 0) {
        downstream_segments_active <-
          count$nextdownidsegment[count$Ntot == count$Ncurrent]
      } else {
        return(rep(FALSE, nrow(rios)))
      }
      activeidx <-
        rios$nextdownidsegment %in% downstream_segments_active
      return(activeidx)
    }

    rios <- object@data
    rios <-
      rios[order(rios$idsegment), ] # As I'm adding later some values from tapply, this is very important
    traveled_segments <- vector()
    upstream_segments <-
      rios$idsegment[(as.logical(rios$issource) &
                        !as.logical(rios$isendoreic))]
    seaidsegments <- rios$idsegment[as.logical(rios$issea)]
    frontieridsegments <-
      rios$idsegment[as.logical(rios$isfrontier) &
                       !is.na(rios$isfrontier)]
    frontieridsegmentsdownstream <-
      rios$nextdownidsegment[as.logical(rios$isfrontier)]
    # need also to remove those segments that will be called as nextdownidsegment
    frontieridsegmentsdownstream <-
      frontieridsegmentsdownstream[!is.na(frontieridsegmentsdownstream)]
    mtchup <- rios$idsegment %in% upstream_segments

    # calculation nsilver in many categories
    rios$wettedsurfaceotherm2[is.na(rios$wettedsurfaceotherm2)] <- 0
    rios$wettedsurfacem2[is.na(rios$wettedsurfacem2)] <- 0
    rios$coeffwettedsurfaceother[is.na(rios$coeffwettedsurfaceother)] <-
      1
    rios$nfemale300450 <-
      rios$pfemale300450 * rios$density * (rios$wettedsurfaceotherm2 * rios$coeffwettedsurfaceother + rios$wettedsurfacem2)
    rios$nfemale450600 <-
      rios$pfemale450600 * rios$density * (rios$wettedsurfaceotherm2 * rios$coeffwettedsurfaceother + rios$wettedsurfacem2)
    rios$nfemale600750 <-
      rios$pfemale600750 * rios$density * (rios$wettedsurfaceotherm2 * rios$coeffwettedsurfaceother + rios$wettedsurfacem2)
    rios$nfemale750 <-
      rios$pfemale750 * rios$density * (rios$wettedsurfaceotherm2 * rios$coeffwettedsurfaceother + rios$wettedsurfacem2)
    rios$nmale150300 <-
      rios$pmale150300 * rios$density * (rios$wettedsurfaceotherm2 * rios$coeffwettedsurfaceother + rios$wettedsurfacem2)
    rios$nmale300450 <-
      rios$pmale300450 * rios$density * (rios$wettedsurfaceotherm2 * rios$coeffwettedsurfaceother + rios$wettedsurfacem2)
    rios$nmale450600 <-
      rios$pmale450600 * rios$density * (rios$wettedsurfaceotherm2 * rios$coeffwettedsurfaceother + rios$wettedsurfacem2)
    rios$nsilver150300 <-
      rios$psilver150300 * rios$density * (rios$wettedsurfaceotherm2 * rios$coeffwettedsurfaceother + rios$wettedsurfacem2)
    rios$nsilver300450 <-
      rios$psilver300450 * rios$density * (rios$wettedsurfaceotherm2 * rios$coeffwettedsurfaceother + rios$wettedsurfacem2)
    rios$nsilver450600 <-
      rios$psilver450600 * rios$density * (rios$wettedsurfaceotherm2 * rios$coeffwettedsurfaceother + rios$wettedsurfacem2)
    rios$nsilver600750 <-
      rios$psilver600750 * rios$density * (rios$wettedsurfaceotherm2 * rios$coeffwettedsurfaceother + rios$wettedsurfacem2)
    rios$nsilver750 <-
      rios$psilver750 * rios$density * (rios$wettedsurfaceotherm2 * rios$coeffwettedsurfaceother + rios$wettedsurfacem2)
    rios$nsilver <-
      rios$psilver * rios$density * (rios$wettedsurfaceotherm2 * rios$coeffwettedsurfaceother + rios$wettedsurfacem2)

    # to check that sums are OK
    # ex(head(cbind(rios[,c("psilver150300","psilver300450", "psilver450600","psilver600750","psilver750")],rios$psilver),50))

    # here I'm running the international script
    # so I don't have values from stream param such as surface... coming from Italy or swizerland... I'm ignoring those for the moment

    # cumulated numbers init with 0

    rios$cnfemale300450 <- 0
    rios$cnfemale450600 <- 0
    rios$cnfemale600750 <- 0
    rios$cnfemale750 <- 0
    rios$cnmale150300 <- 0
    rios$cnmale300450 <- 0
    rios$cnmale450600 <- 0
    rios$cnsilver150300 <- 0
    rios$cnsilver300450 <- 0
    rios$cnsilver450600 <- 0
    rios$cnsilver600750 <- 0
    rios$cnsilver750 <- 0
    rios$cnsilver <- 0

    rios$cnfemale300450[mtchup] <- rios$nfemale300450[mtchup]
    rios$cnfemale450600[mtchup] <- rios$nfemale450600[mtchup]
    rios$cnfemale600750[mtchup] <- rios$nfemale600750[mtchup]
    rios$cnfemale750[mtchup] <- rios$nfemale750[mtchup]
    rios$cnmale150300[mtchup] <- rios$nmale150300[mtchup]
    rios$cnmale300450[mtchup] <- rios$nmale300450[mtchup]
    rios$cnmale450600[mtchup] <- rios$nmale450600[mtchup]
    rios$cnsilver150300[mtchup] <- rios$nsilver150300[mtchup]
    rios$cnsilver300450[mtchup] <- rios$nsilver300450[mtchup]
    rios$cnsilver450600[mtchup] <- rios$nsilver450600[mtchup]
    rios$cnsilver600750[mtchup] <- rios$nsilver600750[mtchup]
    rios$cnsilver750[mtchup] <- rios$nsilver750[mtchup]
    rios$cnsilver[mtchup] <- rios$nsilver[mtchup]

    mtchup <- rios$idsegment %in% upstream_segments
    current_segments <- upstream_segments

    # initialize variables
    traveled_segments <- current_segments
    i <- 0
    level <- 0
    upstream_segments_left <- upstream_segments
    nb <- length(current_segments)
    countforbreakfinal <- 0
    print("sum of upstream segments")
    while (length(current_segments) > 0) {
      level <- level + 1
      i <- length(traveled_segments)
      setWinProgressBar(
        progres,
        i / length(rios$idsegment),
        label = paste(
          "length current seg=",
          length(current_segments),
          "level=",
          level
        )
      )

      downstream_segments_init <-
        rios$nextdownidsegment[mtchup] # this is now downstream 1 rank from the first segments
      if (!is.null(checksegment)) {
        if (checksegment %in% downstream_segments_init) {
          cat(
            sprintf(
              "segment %s is in downstream segments init at level %s \n",
              checksegment,
              level
            )
          )
        }
      }
      # this is already so when level=1
      upstream_segments <- rios$idsegment[mtchup]
      # test <- 'SP298445'%in%downstream_segments_init | 'PT76749'%in%downstream_segments_init

      # we need to remove NA, frontier, the sea idsegments and the frontier idsegments, here we stop
      nonna <- !is.na(downstream_segments_init)
      # if (level==1){
      # allow frontier segments on the first run
      # nonfrontierdownstream<- rep(TRUE, length(downstream_segments_init))
      # } else {
      # 	nonfrontierdownstream <- !downstream_segments_init%in%frontieridsegmentsdownstream
      # }

      keep <-
        nonna # &  nonfrontierdownstream	 #nonsea & nonfrontier &
      # returns an index calculated based on downstream_segments and indicating which downstreamsegments have all ancestors
      activeidx <- fn_idx_active(mtchup, keep, checksegment)
      # from now on only working with those ancestors
      # test <- 'SP298445'%in%rios$nextdownidsegment[activeidx]| 'PT76749'%in%rios$nextdownidsegment[activeidx]
      # 	if (test) browser()



      # this is now downstream from the previous step, in first line only the segments with all ancestors are kept
      downstream_segments <-
        rios$nextdownidsegment[activeidx] # this is now downstream 1 rank from the first segments
      uk_downstream_segments <- unique(downstream_segments)
      # downstream_segments_left<-unique(downstream_segments_all[!downstream_segments_all%in%uk_downstream_segments])

      # upstream segments where all ancestor are found downstream
      # the following are using idsegment as a primary key
      upstream_segments_done <- rios$idsegment[activeidx]
      # upstream segments without all ancestor at this step
      upstream_segments_left <-
        upstream_segments[!upstream_segments %in% upstream_segments_done]


      # mtchdown <- rios$idsegment%in%uk_downstream_segments	# within the entire vector, which are in 'downstream_segments'



      # if (any(is.na(nam))) stop("NA names these should have been removed by the function before : check")

      # calculate cumulated values

      fncalccumul <- function(rios, lab, cumlab) {
        sum_silver_upstream <-
          tapply(rios[, cumlab][activeidx], rios$nextdownidsegment[activeidx], sum,
                 na.rm =
                   TRUE
          )

        nam <- match(names(sum_silver_upstream), rios$idsegment)
        sum_silver_upstream <- sum_silver_upstream[!is.na(nam)]



        silver_current <- rios[, lab][nam]
        silver_current[is.na(silver_current)] <- 0
        silver_current <- silver_current[!is.na(nam)]

        nam <- nam[!is.na(nam)]
        rios[, cumlab][nam] <- sum_silver_upstream + silver_current
        return(rios)
      }

      thelabs <- c(
        "nfemale300450",
        "nfemale450600",
        "nfemale600750",
        "nfemale750",
        "nmale150300",
        "nmale300450",
        "nmale450600",
        "nsilver150300",
        "nsilver300450",
        "nsilver450600",
        "nsilver600750",
        "nsilver750",
        "nsilver"
      )

      thecumlabs <- c(
        "cnfemale300450",
        "cnfemale450600",
        "cnfemale600750",
        "cnfemale750",
        "cnmale150300",
        "cnmale300450",
        "cnmale450600",
        "cnsilver150300",
        "cnsilver300450",
        "cnsilver450600",
        "cnsilver600750",
        "cnsilver750",
        "cnsilver"
      )

      for (i in 1:length(thelabs)) {
        thelab <- thelabs[i]
        thecumlab <- thecumlabs[i]
        rios <- fncalccumul(rios, lab = thelab, cumlab = thecumlab)
      }

      traveled_segments <-
        unique(c(traveled_segments, downstream_segments)) # to change the progress bar in the next loop
      old_current_segments <- current_segments # used for break below
      current_segments <-
        unique(c(uk_downstream_segments, upstream_segments_left))
      nonsea <- !current_segments %in% seaidsegments
      # nonfrontier <- !current_segments%in%frontieridsegments
      current_segments <- current_segments[nonsea]
      mtchup <-
        rios$idsegment %in% current_segments # changing the vector of match to the new level
      oldnb <- nb
      nb <- sum(mtchup)
      if (oldnb == nb &
          identical(old_current_segments, current_segments)) {
        countforbreakfinal <- countforbreakfinal + 1
      }
      if (countforbreakfinal == 2) {
        break()
      }
      print(nb)
    }
    close(progres)
    object@data <- rios
    return(object)
  }
)

#' Method to calculate the smolt production, adapted from stream_order
#' @note the function will select upstream segments and process from the source to the sea
#' the column to fill
#' @return River
#' @author Cedric Briand \email{cedric.briand@@eaux-et-vilaine.bzh}
setGeneric(
  "silver_eel_production",
  def = function(object, ...) {
  })



#' map method
#' @note first load the shp corresponding to bv then plots the variable
#' @return NULL
setMethod(
  "map",
  signature = signature("River"),
  definition = function(object,
                        var = c("shreeve", "strahler")) {
    # cat(str_c("1- loading data from postgis using loadshp method ))
    object <-
      loadshp(object) # remplit le @shp avec juste id_segment et geom
    shp <-
      merge(object@shp, object@data[, c(object@prkey, var)], by = "idsegment")
    layout(matrix(1:2, ncol = 2, nrow = length(var)))
    plot(shp[var], breaks = "hclust")
    # sf.colors(10)
    NULL
  }
)




# object <- rios
setMethod(
  "loadshp",
  signature = signature("River"),
  definition = function(object,
                        geom = "geom") {
    # open connexion for glue_sql
    con <- DBI::dbConnect(
      drv = RPostgres::Postgres(),
      host = getOption("eda.host"),
      dbname = getOption("eda.dbname"),
      user = getOption("eda.user"),
      password = getOption("eda.password")
    )
    on.exit(dbDisconnect(con))
    # name of the layer according to zonegeo
    schema <- object@schema
    table <- "rn"
    table2 <- "rna"
    basin <- object@basin
    emu <- object@emu

    layer <- paste0(schema, ".", table)
    if (length(object@zonegeo > 0)) {
      level_key <-
        c(
          "France" = "FR",
          "Spain" = "SP",
          "Portugal" = "PT"
        )
      zone <- dplyr::recode(object@zonegeo, !!!level_key)
    } else {
      zone <- "all"
    }


    key <- object@prkey

    # query the where clause serves nothing but this way I have two and clauses

    select <-
      glue::glue_sql(
        "select r.{`key`}, {`geom`} from {`schema`}.{`table`} r
							join {`schema`}.{`table2`} r1 ON r.{`key`}=r1.{`key`}
							",
        .con = con
      )
    if (zone != "all") {
      where <- glue::glue_sql(.con = con, "WHERE country= {zone}")
    } else {
      where <- " WHERE TRUE"
    }
    if (length(basin) > 0) {
      and1 <- glue::glue_sql(" AND basin in ({basin*})", .con = con)
    } else {
      and1 <- " AND TRUE"
    }
    if (length(emu) > 0) {
      and2 <- glue::glue_sql(" AND emu in ({emu*})", .con = con)
    } else {
      and2 <- " AND TRUE"
    }

    query <- glue::glue(select, where, and1, and2)
    object@shp <- st_read(
      dsn = con,
      query = query
    )

    dbDisconnect(con)
    return(object)
  }
)
