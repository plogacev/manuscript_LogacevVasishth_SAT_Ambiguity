
library(ggplot2)
library(dplyr)


(load("../../Experiment2_Arg_preprocessed.rda"))

# plot the time course of the proportion of `yes' responses by subject, item and condition
resp_timeline <- group_by(data.arg, subject, time, condition, grammatical) %>% 
                  summarize(response_yes=mean(response_yes))
ggplot(resp_timeline, aes(x=time, y=response_yes, color=condition, group=condition, linetype=as.factor(grammatical)))+facet_wrap(~subject)+geom_point()+geom_line()


# plot the last response proportion of `yes' responses by subject, item and condition
resp_last <- group_by(data.arg, item, condition, grammatical) %>% 
  summarize(response_yes=mean(response_yes_last))
ggplot(resp_last, aes(x=condition, y=response_yes, color=item, group=item))+geom_line()+geom_text(aes(label=item))


### Conditions:
# 1-11-5 a  Was befahl damals der_Chef $ der_Bäckerin?
# 1-11-5 b  Was befahl der_Chef damals $ dem_Bäcker?
# 1-11-5 c  Wem befahl das damals der_Chef $ des_Bäckers?
# 1-11-5 d *Wem befahl das der_Chef damals $ der_Bäckerin?
# 1-11-5 e *Wem befahl das damals der_Chef $ dem_Bäcker?
# 1-11-5 f *Was befahl der_Chef damals $ des_Bäckers?
# 1-11-5 i  Wem befahl das damals $ der_General?
# 1-11-5 j *Was befahl damals $ der_Gedanke?


### compute d'
resp_timeline_dprime <-
  group_by(data.arg, subject, time, last_phrase) %>% 
    summarize(N_gram=length(time[grammatical==1])+1, N_ungram=length(time[grammatical==0])+1,
            response_hits=mean(c(response_yes[grammatical==1],.5)), response_fas=mean(c(response_yes[grammatical==0],.5)))

resp_timeline_dprime$dprime <- with(resp_timeline_dprime, qnorm(response_hits) - qnorm(response_fas))

# plot d' by subject
subset(resp_timeline_dprime, last_phrase!="other") %>%
ggplot(aes(x=time, y=dprime, color=last_phrase, group=last_phrase))+facet_wrap(~subject)+geom_point()+geom_line()

# plot scaled d' by subject
subset(resp_timeline_dprime, last_phrase!="other") %>% group_by(subject, last_phrase) %>%
  mutate(dprime = dprime/max(dprime)) %>% subset(dprime > -1) %>%
  ggplot(aes(x=time, y=dprime, color=last_phrase, group=last_phrase))+facet_wrap(~subject)+geom_point()+geom_line()
