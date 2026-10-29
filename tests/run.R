source("config.R")
for(f in sort(list.files("R",pattern="[.]R$",full.names=TRUE)))source(f)
stopifnot(abs(return_gpd(100,.8,.1,.02,0)-(.8+.02*log(10)))<1e-12)
stopifnot(is.na(return_gpd(5,.8,.1,.02,-.2)))
stopifnot(identical(normalize_name(c("  Lula ","Flávio Bolsonaro")),cfg$candidates))
set.seed(100)
x<-evd::rgpd(5000,scale=.1,shape=-.2)
z<-safe_fit(x,"GPD",.05)
stopifnot(valid_fit(z),abs(z$fit$estimate["shape"]+.2)<.1)
scaled<-safe_fit(100*x,"GPD",5)
stopifnot(valid_fit(scaled),is.finite(z$fit$std.err["shape"]),
 abs(z$fit$std.err["shape"]/scaled$fit$std.err["shape"]-1)<.1,
 abs(100*z$fit$std.err["scale"]/scaled$fit$std.err["scale"]-1)<.1)
g<-threshold_grid(x,cfg)
stopifnot(all(g$excedencias==vapply(g$u,function(u)sum(x>u),integer(1))))
root<-getwd();tmp<-tempfile();dir.create(tmp);setwd(tmp)
for(d in c("tables","data/processed"))dir.create(d,recursive=TRUE)
fixture<-data.table(SG_UF=rep(c("SP","SP","ZZ"),each=2),CD_MUNICIPIO=rep(1:3,each=2),
 NR_ZONA="1",NR_SECAO="1",DS_CARGO="PRESIDENTE",NR_VOTAVEL=rep(c("13","22"),3),
 NM_VOTAVEL=rep(unname(cfg$candidate_names),3),QT_VOTOS=c(100,50,0,0,10,20),NR_TURNO="2",ANO_ELEICAO="2026")
fwrite(fixture,"input.csv",sep=";");conf<-cfg;conf$input<-"input.csv";conf$encoding<-"UTF-8"
a<-prepare_data(conf)
stopifnot(nrow(a)==2,all(a$SG_UF=="SP"),all(a$total_validos==150),abs(sum(a$p)-1)<1e-10)
fwrite(rbind(fixture,fixture[1]),"input.csv",sep=";")
stopifnot(inherits(try(prepare_data(conf),silent=TRUE),"try-error"))
fwrite(fixture[-2],"input.csv",sep=";")
a<-prepare_data(conf);stopifnot(nrow(a)==0)
setwd(root);unlink(tmp,recursive=TRUE)
message("Testes de integridade, suporte e estimação concluídos.")
