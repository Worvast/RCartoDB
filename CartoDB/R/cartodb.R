cartodb <-
function(account.name, api.key = NULL, api.sql = NULL, api.tiles = NULL, api.maps = NULL) {

    if(is.null(.CartoDB$data)) {
        .CartoDB<-new.env()
        .CartoDB$data<-list()
    }

    .CartoDB$data <- list(
        api.key=NULL,
        account.name="",
        api.sql=NULL,
        api.tiles=NULL,
        api.maps=NULL
        )

    if(is.character(account.name)){
        .CartoDB$data$account.name<-account.name
    } else{
        warning("Account name must be a string")
        account.name<-""
    }

    if(is.character(api.sql)){
        .CartoDB$data$api.sql<-api.sql
    } else {
        .CartoDB$data$api.sql<-paste(account.name,".cartodb.com/api/v2/sql",sep="")
        print("No custom SQL API url")
    }
    print(paste("SQL Api: ", .CartoDB$data$api.sql, sep= ""))

    if(is.character(api.tiles)){
        .CartoDB$data$api.tiles<-api.tiles
    } else {
        .CartoDB$data$api.tiles<-paste(account.name,".cartodb.com/tiles/",sep="")
        print("No custom API Tiles url")
    }
    print(paste("Tiles Api: ", .CartoDB$data$api.tiles, sep= ""))

    if(is.character(api.maps)){
        .CartoDB$data$api.maps<-api.maps
    } else {
        .CartoDB$data$api.maps<-paste(account.name,".cartodb.com/tables/",sep="")
        print("No custom API MAPS url")
    }
    print(paste("Maps Api: ", .CartoDB$data$api.maps, sep= ""))
    
    if(is.character(api.key)){
        .CartoDB$data$api.key<-api.key   
    } else{
        warning("Without an API key you are limited to read-only")
    }
}

cartodb.test <- function() {
    url <- cartodb.sql.base()
    warning(url)
    records<-getURL(URLencode(paste(url,"q=SELECT 1",sep='')))
    json<-fromJSON(records[[1]])
    response<-data.frame(success=FALSE)
    if ( 'rows' %in% names(json)) {
        response$success = TRUE
    } else {
        warning(paste("Be sure that your account name,",.CartoDB$data$account.name, "is spelled correctly and that you have a working connection to the internet."))
        response$success = FALSE
    }
    return(response)
}

cartodb.transformGeom<-
function(option=NULL) {
    if (is.null(option)) { return('the_geom') }
    else if (option=="XY") { return('ST_X(the_geom) AS the_geom_x, ST_Y(the_geom) AS the_geom_y,null as the_geom') }
    else if (option=="WKT") { return('ST_AsText(the_geom) AS the_geom') }
    else if (option=="GeoJSON") { return('ST_AsGeoJSON(the_geom) AS the_geom') }
    else if (option=="WKB") { return('the_geom as the_geom') }
    else if (option=="the_geom") { return('the_geom') }
    else { return('ST_X(the_geom) AS the_geom_x, ST_Y(the_geom) AS the_geom_y,null as the_geom') }
}