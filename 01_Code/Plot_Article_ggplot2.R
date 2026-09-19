# Article plots in the submitted paper's ggplot2 style, from audited Python results.
# Run: Rscript 01_Code/Plot_Article_ggplot2.R [path/to/results]
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(jsonlite))
args_all<-commandArgs(trailingOnly=FALSE)
script<-sub('^--file=','',args_all[grep('^--file=',args_all)][1])
root<-dirname(dirname(normalizePath(script)))
extra<-commandArgs(trailingOnly=TRUE)
results<-if(length(extra)) normalizePath(extra[1]) else file.path(root,'03_Results')
figdir<-file.path(root,'04_Figures');plotdir<-file.path(figdir,'plot_data')
paperdir<-file.path(root,'05_Manuscript','figures')
raw<-read.csv(file.path(results,'metrics_by_seed.csv'),stringsAsFactors=FALSE)
arch<-c('Simple-A','Simple-B','Multilayer','Multilayer-static')
colours<-c('Simple-A'='#F8766D','Simple-B'='#00BA38','Multilayer'='#619CFF','Multilayer-static'='#777777')
shapes<-c('Simple-A'=16,'Simple-B'=17,'Multilayer'=15,'Multilayer-static'=18)
types<-c('Simple-A'='solid','Simple-B'='dashed','Multilayer'='dotted','Multilayer-static'='dotdash')
style<-theme_gray(base_size=12,base_family='sans')+
  theme(legend.position='bottom',legend.box='vertical',plot.margin=margin(6,9,6,6),
        legend.key.width=grid::unit(1.2,'lines'),legend.text=element_text(size=9.5),
        legend.title=element_text(size=10),axis.text=element_text(size=10),
        axis.title=element_text(size=11),strip.text=element_text(size=10.5),
        panel.spacing.x=grid::unit(.8,'lines'))
scales_arch<-function(drop=FALSE) list(scale_colour_manual(values=colours,drop=drop),
  scale_shape_manual(values=shapes,drop=drop),scale_linetype_manual(values=types,drop=drop))
decorate<-function(d) {
  d$Dataset<-factor(d$dataset,levels=c('Carclaims','InsuranceClaims'),labels=c('Carclaims','Insurance Claims'))
  if('architecture' %in% names(d)) d$Method<-factor(d$architecture,levels=arch)
  d
}
long<-function(d,metrics,labels,percent=TRUE) {
  q<-do.call(rbind,lapply(seq_along(metrics),function(i) {
    x<-d;x$Metric<-labels[i];x$Value<-x[[metrics[i]]] * if(percent) 100 else 1;x
  }))
  q$Metric<-factor(q$Metric,levels=labels);decorate(q)
}
summarise_plot<-function(d,by) {
  a<-aggregate(d$Value,d[by],mean);names(a)[ncol(a)]<-'Mean'
  b<-aggregate(d$Value,d[by],sd);names(b)[ncol(b)]<-'SD'
  merge(a,b,by=by,sort=FALSE)
}
manifest<-list()
save_plot<-function(p,name,width=6.5,height=3.4) {
  p<-p+style
  for(ext in c('png','pdf','svg','eps')) {
    final_out<-file.path(figdir,paste0(name,'.',ext))
    out<-tempfile(pattern=paste0(name,'_'),fileext=paste0('.',ext))
    if(ext=='png') ggsave(out,p,width=width,height=height,units='in',dpi=720,bg='white',type='cairo')
    if(ext=='pdf') ggsave(out,p,width=width,height=height,units='in',device=cairo_pdf,bg='white')
    if(ext=='svg') ggsave(out,p,width=width,height=height,units='in',device=svglite::svglite,bg='white')
    if(ext=='eps') ggsave(out,p,width=width,height=height,units='in',device=cairo_ps,bg='white',onefile=FALSE)
    stopifnot(file.info(out)$size>100)
    pending<-paste0(final_out,'.pending')
    stopifnot(file.copy(out,pending,overwrite=TRUE))
    if(file.exists(final_out)) unlink(final_out)
    stopifnot(file.rename(pending,final_out))
    unlink(out)
  }
  stopifnot(file.copy(file.path(figdir,paste0(name,'.png')),paperdir,overwrite=TRUE))
  md5<-tools::md5sum(file.path(figdir,paste0(name,c('.png','.pdf','.svg','.eps'))))
  names(md5)<-basename(names(md5))
  manifest[[name]]<<-list(engine='R / ggplot2',png_dpi=720,width_inches=width,height_inches=height,files_md5=as.list(md5))
  cat('Rendered',name,'\n')
}
pooled<-raw[raw$setting=='Main' & raw$partition=='D2-D4',]
main<-pooled[pooled$architecture %in% arch,]

d<-long(main,c('recall','f1','pr_auc'),c('Fraud recall','Fraud F1','Average precision'))
a<-summarise_plot(d,c('Dataset','Method','Metric'))
write.csv(a,file.path(plotdir,'ggplot_architecture_summary.csv'),row.names=FALSE)
p<-ggplot(a,aes(Method,Mean,colour=Method,shape=Method))+
  geom_errorbar(aes(ymin=pmax(0,Mean-SD),ymax=pmin(100,Mean+SD)),width=.14)+
  geom_point(size=2)+facet_grid(Dataset~Metric)+scales_arch()+
  scale_x_discrete(labels=c('A','B','Multi','Static'))+
  scale_y_continuous(breaks=seq(0,80,20))+coord_cartesian(ylim=c(0,80))+
  labs(x='Structure',y='Percent',colour='Method',shape='Method')
save_plot(p,'02_architecture_performance')

parts<-raw[raw$setting=='Main' & raw$architecture %in% arch & raw$partition %in% c('D2','D3','D4'),]
d<-long(parts,c('f1','recall'),c('Fraud F1','Fraud recall'));d$Partition<-as.integer(sub('D','',d$partition))
a<-summarise_plot(d,c('Dataset','Method','Metric','Partition'))
write.csv(a,file.path(plotdir,'ggplot_partition_summary.csv'),row.names=FALSE)
p<-ggplot(a,aes(Partition,Mean,colour=Method,shape=Method,linetype=Method))+
  geom_line(linewidth=.55)+geom_point(size=2)+
  geom_errorbar(aes(ymin=pmax(0,Mean-SD),ymax=pmin(100,Mean+SD)),width=.07,linewidth=.35)+
  facet_grid(Dataset~Metric)+scales_arch()+scale_x_continuous(breaks=2:4)+
  scale_y_continuous(breaks=seq(0,80,20))+coord_cartesian(ylim=c(0,80))+
  labs(x='Partition',y='Percent')
save_plot(p,'03_partition_progress')

# Preserve the original batch-wise line/point approach for workload.
d<-long(parts,c('alert_rate','review_rate'),c('Alerts','Total review'));d$Partition<-as.integer(sub('D','',d$partition))
a<-summarise_plot(d,c('Dataset','Method','Metric','Partition'))
write.csv(a,file.path(plotdir,'ggplot_workload_summary.csv'),row.names=FALSE)
p<-ggplot(a,aes(Partition,Mean,colour=Method,shape=Method,linetype=Method))+
  geom_line(linewidth=.55)+geom_point(size=2)+facet_grid(Dataset~Metric)+
  scales_arch()+scale_x_continuous(breaks=2:4)+
  scale_y_continuous(breaks=seq(0,50,10))+coord_cartesian(ylim=c(0,55))+
  labs(x='Partition',y='Percent of claims')
save_plot(p,'04_workload')

models<-c('LR','SVM','XGB','NB','RF','KNN','AdaBoost','ANN','BalancedRF','PSO-XGB-adapted','Simple-B')
d<-long(pooled[pooled$architecture %in% models,],c('f1','pr_auc'),c('Fraud F1','Average precision'))
d$Classifier<-factor(d$architecture,levels=rev(models))
a<-summarise_plot(d,c('Dataset','Classifier','Metric'))
write.csv(a,file.path(plotdir,'ggplot_baseline_summary.csv'),row.names=FALSE)
p<-ggplot(a,aes(Classifier,Mean,colour=Metric,shape=Metric))+
  geom_errorbar(aes(ymin=pmax(0,Mean-SD),ymax=pmin(100,Mean+SD)),position=position_dodge(.45),width=.3)+
  geom_point(position=position_dodge(.45),size=2)+facet_wrap(~Dataset,nrow=1)+
  coord_flip(ylim=c(0,80))+scale_y_continuous(breaks=seq(0,80,20))+
  labs(x='Classifier',y='Percent',colour='Metric',shape='Metric')
save_plot(p,'05_baseline_comparison',height=4.7)

d<-decorate(read.csv(file.path(plotdir,'representative_curves_seed1234.csv')))
d$Curve<-factor(d$curve,levels=c('ROC','PR'),labels=c('ROC','Precision-recall'))
p<-ggplot(d,aes(x,y,colour=Method,linetype=Method))+
  geom_path(linewidth=.6)+facet_grid(Dataset~Curve)+scales_arch()+
  scale_x_continuous(limits=c(0,1))+scale_y_continuous(limits=c(0,1))+
  labs(x='False-positive rate (ROC) / Recall (PR)',y='True-positive rate (ROC) / Precision (PR)')
refs<-data.frame(Dataset=factor(c('Carclaims','Insurance Claims'),levels=levels(d$Dataset)),
  Curve=factor('ROC',levels=levels(d$Curve)),slope=1,intercept=0)
prev<-data.frame(Dataset=factor(c('Carclaims','Insurance Claims'),levels=levels(d$Dataset)),
  Curve=factor('Precision-recall',levels=levels(d$Curve)),prevalence=c(0,0))
for(i in seq_len(nrow(prev))) {
  q<-read.csv(gzfile(file.path(results,c('Carclaims','InsuranceClaims')[i],'1234','Main','simple_a','predictions.csv.gz')))
  q<-q[q$model=='Simple-A' & q$partition!='D1',];prev$prevalence[i]<-mean(q$y)
}
p<-p+geom_abline(data=refs,aes(slope=slope,intercept=intercept),colour='grey45',linetype='dashed')+
  geom_hline(data=prev,aes(yintercept=prevalence),colour='grey45',linetype='dashed')
save_plot(p,'06_roc_pr',height=4.4)

d<-read.csv(file.path(plotdir,'paired_ablation_deltas.csv'))
d<-long(d,c('f1','pr_auc','review_rate'),c('F1 change','AP change','Review-rate change'))
a<-summarise_plot(d,c('Dataset','Metric'))
write.csv(a,file.path(plotdir,'ggplot_ablation_summary.csv'),row.names=FALSE)
p<-ggplot(d,aes(Dataset,Value,colour=Dataset,shape=Dataset))+
  geom_hline(yintercept=0,colour='grey45',linetype='dashed')+
  geom_point(position=position_jitter(width=.10,height=0,seed=1234),size=1.8,alpha=.7)+
  geom_errorbar(data=a,mapping=aes(x=Dataset,ymin=Mean-SD,ymax=Mean+SD),inherit.aes=FALSE,width=.12,colour='black')+
  geom_point(data=a,aes(Dataset,Mean),inherit.aes=FALSE,shape=18,size=2.5,colour='black')+
  facet_wrap(~Metric,nrow=1,scales='free_y')+
  scale_x_discrete(labels=c('Carclaims','Insurance\nClaims'))+
  labs(x='Dataset',y='Change (percentage points)')
save_plot(p,'07_ablation_deltas',height=3.4)

d<-raw[raw$architecture=='Multilayer' & raw$partition=='D2-D4',]
d<-long(d,c('recall','f1','pr_auc','review_rate'),c('Fraud recall','Fraud F1','Average precision','Total review'))
d$Setting<-factor(d$setting,levels=c('Main','Strict'),labels=c('(3,4)','(5,6)'))
a<-summarise_plot(d,c('Dataset','Metric','Setting'))
write.csv(a,file.path(plotdir,'ggplot_threshold_summary.csv'),row.names=FALSE)
p<-ggplot(a,aes(Setting,Mean,group=Dataset,colour=Dataset,shape=Dataset,linetype=Dataset))+
  geom_line(linewidth=.55)+geom_point(size=2)+
  geom_errorbar(aes(ymin=pmax(0,Mean-SD),ymax=pmin(100,Mean+SD)),width=.08)+
  facet_grid(Dataset~Metric)+scale_y_continuous(breaks=seq(0,80,20))+
  coord_cartesian(ylim=c(0,80))+
  labs(x=expression(paste('Joint setting (',T[AD],', ',T[ML],')')),y='Percent')
save_plot(p,'08_threshold_sensitivity',height=3.4)

d<-decorate(read.csv(file.path(plotdir,'supervised_fit_seconds.csv')));d$Value<-d$seconds
a<-summarise_plot(d,c('Dataset','Method'))
write.csv(a,file.path(plotdir,'ggplot_runtime_summary.csv'),row.names=FALSE)
p<-ggplot(a,aes(Method,Mean,colour=Method,shape=Method))+
  geom_errorbar(aes(ymin=pmax(0,Mean-SD),ymax=Mean+SD),width=.13)+geom_point(size=2.2)+
  facet_wrap(~Dataset,nrow=1,scales='free_y')+scales_arch(drop=TRUE)+
  labs(x='Structure',y='Supervised fitting time (seconds)')
save_plot(p,'09_runtime',height=3.2)

writeLines(capture.output(sessionInfo()),file.path(root,'08_Documentation','R_ggplot2_sessionInfo.txt'))
write_json(list(status='PASS',implementation='Actual executed R / ggplot2',
  R_version=R.version.string,ggplot2_version=as.character(packageVersion('ggplot2')),
  script_md5=unname(tools::md5sum(script)),
  results_directory=basename(results),
  input_md5=as.list(tools::md5sum(file.path(results,'metrics_by_seed.csv'))),
  original_style_source='07_Original_Archive/Submitted_V5_Confirmed/Pictures/FiguresCode.R',
  scope='Same audited Python results; no re-fitting or new threshold grid. Original workflow PNGs are copied verbatim.',
  axis_ranges=list(performance_percent=c(0,80),workload_percent=c(0,55),ROC_PR=c(0,1)),
  figures=manifest),file.path(root,'08_Documentation','ggplot2_build_report.json'),pretty=TRUE,auto_unbox=TRUE)
