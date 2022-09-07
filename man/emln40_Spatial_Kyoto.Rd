\name{emln40_Spatial_Kyoto}
\alias{emln40_Spatial_Kyoto}
\docType{data}
\title{emln Multilayer network no. 40}
\description{
A spatial multilayer network that is comprised out of eight papers documenting pollination networks in differing coordinates.
}
\usage{load_emln(40/emln40_Spatial_Kyoto)}
\format{
  The format is:
List of 4


 $ description : spec_tbl_df [9 × 2] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
  ..$ attribute: chr [1:9] "data_entry" "multilayer_network_type" "description" "source" ...
  ..$ value    : chr [1:9] "ofir_segev" "Spatial" NA "Web_of_Life" ...
  ..- attr(*, "spec")=
  .. .. cols(
  .. ..   attribute = col_character(),
  .. ..   value = col_character()
  .. .. )
  ..- attr(*, "problems")=<externalptr>


 $ references  : spec_tbl_df [8 × 5] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
  ..$ doi      : logi [1:8] NA NA NA NA NA NA ...
  ..$ author   : chr [1:8] "KATO, Makoto" "KATO, Makoto; KAKUTANI, Takehiko; INOUE, Tamiji; ITINO, Takao" "KATO, Makoto; KAKUTANI, Takehiko; INOUE, Tamiji; ITINO, Takao" "INOUE, Tamiji; KATO, Makoto; KAKUTANI, Takehiko; SUKA, Takeshi; ITINO, Takao" ...
  ..$ year     : Date[1:8], format: "2000-03-31" "1990-08-20" "1990-08-20" ...
  ..$ paper_url: chr [1:8] "http://hdl.handle.net/2433/156116" "http://hdl.handle.net/2433/156101" "http://hdl.handle.net/2433/156101" "http://hdl.handle.net/2433/156100" ...
  ..$ data_url : chr [1:8] "http://www.web-of-life.es/map.php" "http://www.web-of-life.es/map.php" "http://www.ecologia.ib.usp.br/iwdb/resources.html" "http://www.web-of-life.es/map.php" ...
  ..- attr(*, "spec")=
  .. .. cols(
  .. ..   doi = col_logical(),
  .. ..   author = col_character(),
  .. ..   year = col_date(format = ""),
  .. ..   paper_url = col_character(),
  .. ..   data_url = col_character()
  .. .. )
  ..- attr(*, "problems")=<externalptr>


 $ nodes       : spec_tbl_df [11,624 × 3] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
  ..$ node_id  : num [1:11624] 1 1 1 1 2 2 2 2 3 3 ...
  ..$ attribute: chr [1:11624] "node_name" "type" "taxa_verified" "verification_level" ...
  ..$ value    : chr [1:11624] "Castanopsis_sieboldii" "plant" "TRUE" "species" ...
  ..- attr(*, "spec")=
  .. .. cols(
  .. ..   node_id = col_double(),
  .. ..   attribute = col_character(),
  .. ..   value = col_character()
  .. .. )
  ..- attr(*, "problems")=<externalptr>


 $ interactions: spec_tbl_df [41,412 × 3] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
  ..$ interaction_id: num [1:41412] 1 1 1 1 1 1 2 2 2 2 ...
  ..$ attribute     : chr [1:41412] "node_from" "layer_from" "node_to" "layer_to" ...
  ..$ value         : chr [1:41412] "Castanopsis_sieboldii" "Amami-Ohsima_Island" "Stomorhina.obsoleta." "Amami-Ohsima_Island" ...
  ..- attr(*, "spec")=
  .. .. cols(
  .. ..   interaction_id = col_double(),
  .. ..   attribute = col_character(),
  .. ..   value = col_character()
  .. .. )
  ..- attr(*, "problems")=<externalptr>
 - attr(*, "class")= chr "EMLN"
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
load_emln(emln40_Spatial_Kyoto)
## maybe str(emln40_Spatial_Kyoto) ; plot(emln40_Spatial_Kyoto) ...
}
\keyword{datasets}
