#### This file contains only
#### Functions for other scripts
#### source() this file in other scripts for better readability

library(tidyverse)

### sample a exploration time from the distribution, if it is negative, then re-sample
explengthsample <- function(expN,expVar){
  out = rnorm(1,expN,expVar)
  while (out <= 0)
    out = rnorm(1,expN,expVar)
  end
  out
}

### calculate the log function
log_inve <- function (x,a,h=0){
  if ((1/x)-1 > 0){
    out = ((log((1/x)-1)/-a) + h)
  }else if ((1/x)-1 < 0){
    out = ((-log(-((1/x)-1))/-a) + h)
  }else if ((1/x)-1 == 0){
    out = ((0/-a) + h)
  }
  out
}

### calculate the bonus based on the confidence
Bonus <- function (Con){
  if (Con >= 0.75){
    out = 3 * Con
  }else if(Con >= 0.6){
    out = 2 * Con
  }else{
    out = Con
  }
  out
}

## recovery status changes over time

## compute accuracy and feedback
type <- function(risk, Schema_res, Item_EI, Outputs_cho, ThisPhase, schemainfo){
  if (risk == 'high'){ #  high risk
    if (length(unique(Schema_res)) == 1){
      AC = 1
      performance = sum(Item_EI$payoff[Item_EI$ID %in% Outputs_cho[ThisPhase,c("Cho_1","Cho_2","Cho_3","Cho_4")]]) * 3
    }else if(length(unique(Schema_res)) == 2 & sum(Schema_res == unique(Schema_res)[1]) != 2){ #受试者选择了 2 种 schema，且 schema 不是 2:2 分布
      # 3/4
      AC = 0.5
      performance = sum(Item_EI$payoff[Item_EI$ID %in% Outputs_cho[ThisPhase,c("Cho_1","Cho_2","Cho_3","Cho_4")]])
    }else{
      AC = 0
      performance = sum(Item_EI$payoff[Item_EI$ID %in% Outputs_cho[ThisPhase,c("Cho_1","Cho_2","Cho_3","Cho_4")]])
    }
  }else{ ## low risk
    if (length(unique(Schema_res)) == 1){
      AC = 1
      performance = sum(Item_EI$payoff[Item_EI$ID %in% Outputs_cho[ThisPhase,c("Cho_1","Cho_2","Cho_3","Cho_4")]]) * 3
    }else if(length(unique(Schema_res)) == 2 & sum(Schema_res == unique(Schema_res)[1]) != 2){
      AC = 0.5
      right_schema =  as.data.frame(sort(table(Schema_res), decreasing=TRUE))[1,1] # most frequent schema chosen
      performance = sum(Item_EI$payoff[Item_EI$ID %in% Outputs_cho[ThisPhase,c("Cho_1","Cho_2","Cho_3","Cho_4")]]) + schemainfo$payoff[schemainfo$schemaID==right_schema]*3
    }else{
      AC = 0
      performance = sum(Item_EI$payoff[Item_EI$ID %in% Outputs_cho[ThisPhase,c("Cho_1","Cho_2","Cho_3","Cho_4")]])
    }
  }
  
  # mean payoff
  mean.payoff <- sum(Item_EI$payoff[Item_EI$ID %in% 
                                      Outputs_cho[ThisPhase,
                                                  c("Cho_1","Cho_2","Cho_3","Cho_4")]]) / 4
  
  return(list(AC, performance, mean.payoff))
}



simulation <- function(Param.df, type, exp_type, save=F, savepath="",
                       sim.mode=c("before", "after", "whole")[3], 
                       before.path=NA,
                       save.confi=F,
                       after.read.best.only=F,
                       model.version=c(1,2,3)[1], # 0817: 1 is now the old version
                       scale.confi.init){
  ## df initialization
  subjectnum = nrow(Param.df)
  simupath = file.path(savepath, type)
  expschema = read.csv(paste0("data/", exp_type, "_schemainfo2.csv"))
  if(scale.confi.init){ # 0817
    for(row in 1:nrow(expschema)){
      curr.payoff <- expschema$payoff[row]
      if(curr.payoff == 2){
        expschema$familirarity[row] <- 0.9
      }else if(curr.payoff == 3){
        expschema$familirarity[row] <- 0.6
      }else if(curr.payoff == 4){
        expschema$familirarity[row] <- 0.4
      }else if(curr.payoff == 5){
        expschema$familirarity[row] <- 0.3
      }else if(curr.payoff == 6){
        expschema$familirarity[row] <- 0.2
      }
    }
  }
  print(expschema)
  
  max_Round = 200 ## large enough to create initial dataframe
  ALL.df = data.frame()
  confidence.df <- data.frame()
  gconfidence.df <- data.frame()
  dwelltime.df <- data.frame()
  
  ## subject-wise simulation
  for (Subject in 1:subjectnum) {
    error = 0
    start.time <- Sys.time()
    
    ## assign parameter
    a_schema = Param.df$a_schema[Subject]
    h_schema = Param.df$h_schema[Subject]
    Beta_N = Param.df$Beta_N[Subject]
    Beta_Var = Param.df$Beta_Var[Subject]
    a_generic = Param.df$a_generic[Subject]
    h_generic = Param.df$h_generic[Subject]
    Beta_gN = Param.df$Beta_gN[Subject]
    Beta_gVar = Param.df$Beta_gVar[Subject]
    w = Param.df$w[Subject]
    Phi = Param.df$Phi[Subject]
    decay_speed = Param.df$decay_speed[Subject]
    decay_speed_thres = Param.df$decay_speed_thres[Subject]
    thres_item_inter = Param.df$thres_item_inter[Subject]
    thres_item_final = Param.df$thres_item_final[Subject]
    # thres_schema = Param.df$thres_schema[Subject] # should be modiflied later
    theta_shift = Param.df$theta_shift[Subject]
    timevar = Param.df$timevar[Subject]
    modeltimestep = Param.df$modeltimestep[Subject]
    
    
    ##### except for the schema selection part, other parts are the same as what we discussed
    #### create a data frame to store the outputs ####
    
    Outputs_cho = data.frame(Subject = c(rep(Subject, max_Round*2)), Round = c(rep(1:(max_Round*2),each=2)),Phase = c(1:(max_Round*2)),
                             Schema = c(rep(0,max_Round*2)),Schema_RT = c(rep(0,max_Round*2)),
                             Schema_OB = c(rep(0,max_Round*2)),Schema_AS = c(rep(0,max_Round*2)),
                             Cho_1 = c(rep(0,max_Round*2)),Cho_2 = c(rep(0,max_Round*2)), Cho_3 = c(rep(0,max_Round*2)), 
                             Cho_4 = c(rep(0,max_Round*2)),RT_1 = c(rep(0,max_Round*2)), RT_2 = c(rep(0,max_Round*2)), 
                             RT_3 = c(rep(0,max_Round*2)), RT_4 = c(rep(0,max_Round*2)),OB_1 = c(rep(0,max_Round*2)), 
                             OB_2 = c(rep(0,max_Round*2)), OB_3 = c(rep(0,max_Round*2)), OB_4 = c(rep(0,max_Round*2)), 
                             AS_1 = c(rep(0,max_Round*2)), AS_2 = c(rep(0,max_Round*2)), AS_3 = c(rep(0,max_Round*2)), 
                             AS_4 = c(rep(0,max_Round*2)),
                             schema_payoff = c(rep(0,max_Round*2)), AC = c(rep(0,max_Round*2)), 
                             performance = c(rep(0,max_Round*2)),afterbreak = c(rep(0,max_Round*2)))
    Outputs_learn = data.frame()
    Outputs_glearn = data.frame()
    ## new add dwell table 
    Outputs_dwell = data.frame() 
    
    ##### create the experiment for further simulation ######
    
    ### new firstly, the schemainfo is based on experiment data ###
    # schemainfo contains conN (mean of confidence), 
    # conVar (variance of confidence), 
    # expN (mean of exploration time) and 
    # expVar (variance of exploration time)
    schemainfo <- expschema  %>% filter(new==0) %>% select(-c(author,new))
    colnames(schemainfo)[c(3,5,6)] = c('conN','expN','expVar')# 
    ## reset familiarity 
    # make the payoff always from 6 to 2
    schemainfo = arrange(schemainfo, desc(payoff))
    # 0.2 = 2, 0.35 = 3, 0.5 = 4, 0.65 = 5, 0.8 = 6
    # confidence = 0.2 + 0.8 * familiarity
    # schemainfo$conN = c(0.2,0.2,0.35,0.35,0.5,0.5,0.65,0.65,0.8,0.8)
    # schemainfo$conN = (0.6-0.2)/(0.8-0.2)*(schemainfo$conN-0.2)+0.2
    
    if(!scale.confi.init){
      schemainfo$conN = 0.2+0.8*schemainfo$conN # 0817
    }
    
    # here we use the individual variance, so that the initial value of variance for each schema are the same
    schemainfo$conVar = c(rep(var(schemainfo$conN), 10))
    ### the exploration length is sampled from the experimental data for each schema 
    # schemainfo$expN = c(3.3681, 2.3268, 2.4154, 3.4589, 2.8481, 2.8867, 2.2546, 2.5310, 2.4061, 2.3860)
    # schemainfo$expVar = c(53.6274, 91.8533, 47.8284, 107.0112, 73.0742, 131.9017, 59.3601, 174.0061, 83.4434, 18.1462)
    
    ### Secondly, the presented item in exploration phase are generated ###
    # Expitem has a col for item's ID and a col for for schema's ID
    Expitem = data.frame(itemID = c(1:240))
    Expitem$SchemaID = rep(schemainfo$schemaID, each=24)
    
    # Decitem has a col for item's ID, a col for schema's ID and a col for payoff
    Decitem = data.frame(ItemID = c(1:120))
    Decitem$SchemaID = rep(schemainfo$schemaID,each=12)
    Decitem$payoff = rep(schemainfo$payoff,each=12)
    
    # initialize the generic confidence
    Gcon = data.frame(conN = mean(schemainfo$conN), conVar = mean(schemainfo$conVar))
    
    ##### let's start the experiment #####
    ThisRound = 1
    clock = 0 # record time, in second
    break_happens = 0 ## flag to indicate break happens
    explorationtime = 600 ## the exploration time (s), 10 min for first exploration
    if (type %in% c("L","Lc","LH")){## i.e. low risk before break
      risk = "low"
    } else {
      risk = "high"
    }
    
    # we don't want to be trapped in time when estimating "after", so we set a flag
    not.updated <- T
    
    ##### when the agent has not finished the experiment, the loop continue as usual
    ##### out only when time is over
    while (clock < 4500){# new clock < 4500 i.e. total time is (85-10)*60 seconds
      print(paste("Subject:", Subject,"Round:", ThisRound,"Time:", clock))
      print(paste("break_happens:",break_happens,"risk:",risk,"type:", type))
      
      
      # artificial clock signal if we only simulate afterbreak
      if(sim.mode == "after" & not.updated){
        clock <- 1500
        not.updated <- F
      }
      
      ## BREAK LOGIC
      if (break_happens == 0 & clock >= 1500){
        
        # if we want to simulate after: we import the last set of confidence for each Subject
        if(sim.mode=="after"){
          ref.schemainfo <- read.csv(file.path(before.path, "confidence.csv"))
          ref.gconfidence.df <- read.csv(file.path(before.path, "gconfidence.csv"))
          
          if(after.read.best.only){
            # if we use best one, we just use that one
            target.schemainfo <- ref.schemainfo
            target.Gcon <- ref.gconfidence.df
            
          }else{ # this may be deprecated!
            random.subj <- Subject
            if(!(random.subj  %in% ref.schemainfo$Subject)){
              # if before simulation crashed, we exit with error 1
              print('before part of estimation nonexistent, continued')
              error <- 1
              break
            }
            
            # schema confidence
            target.schemainfo <- ref.schemainfo %>%
              filter(Subject == random.subj)
            
            # generic confidence
            target.Gcon <- ref.gconfidence.df %>%
              filter(Subject == random.subj) %>%
              select(-Subject)
          }
          
          # now we fill them to the model
          schemainfo <- target.schemainfo %>%
            filter(Time == max(target.schemainfo$Time)) %>%
            select(-c(Time, Subject))
          Gcon <- target.Gcon[nrow(target.Gcon),]
          
        }
        
        ### introduce new schema here
        if (type %in% c("L","H")){
          schemainfo2 <- expschema %>% filter(type=='new') %>% select(-c(author,new)) #只加type=new的
          colnames(schemainfo2)[c(3,5,6)] = c('conN','expN','expVar')
          ## reset familiarity 
          schemainfo2 = arrange(schemainfo2,desc(payoff))
          # schemainfo2$conN = c(0.2,0.35,0.5,0.65,0.8) ## set to default value
          schemainfo2$conN = 0.2+0.8*schemainfo2$conN
          # here we use the individual variance, so that the initial value of variance for each schema are the same
          schemainfo2$conVar = c(rep(var(schemainfo2$conN),5))
          # combine with the same (unchanged) schema
          if("X" %in% colnames(schemainfo)){
            schemainfo <- schemainfo %>% filter(type=='same') %>% select(-c("X")) ############THIS IS ALL WE NEED FROM "BEFORE"
          }else{
            schemainfo <- schemainfo %>% filter(type=='same')
          }
          schemainfo <- rbind(schemainfo,schemainfo2)
          # make the payoff always from 6 to 2
          schemainfo <- arrange(schemainfo,desc(payoff))
          # item should also be changed, as new schema is introduced
          Expitem$SchemaID = rep(schemainfo$schemaID,each=24)
          Decitem$SchemaID = rep(schemainfo$schemaID,each=12)
          Decitem$payoff = rep(schemainfo$payoff,each=12)
        }
        
        ### change risk
        if (type %in% c("L","Lc","HL")){## i.e. low risk after break
          risk = "low"
        } else {
          risk = "high"
        }
        
        ## mark break flag as 1, indicating after break
        print(paste0("break at ", clock))
        break_happens = 1
        breakR = ThisRound ## breakR is the first round after break
        explorationtime = 300 ## exploration time is 5 min right after break
        
        # if we only want to estimate params before break, we stop here
        if(sim.mode == "before"){
          break
        }
      }
      
      
      
      
      
      #### firstly, go through the exploration phase 
      ## firstly, sample the confidence prior the exploration
      Con_PriorExp = mapply(rnorm, 1, mean = schemainfo$conN, sd = schemainfo$conVar)
      
      ### we then sample the exploration length
      Exptime = mapply(explengthsample, expN = schemainfo$expN, expVar = schemainfo$expVar)
      
      ### new: scale exploration time
      Exptime = Exptime  * explorationtime/sum(Exptime) * 0.6 # rescale time 
      clock = clock + explorationtime
      explorationtime = 180 ## usually, the exploration time is 3 min
      
      Outputs_learn = rbind(Outputs_learn,schemainfo)
      Outputs_glearn = rbind(Outputs_glearn,Gcon)
      
      ### calculate the prior learning progress based on a inverse formula 
      learning = mapply(log_inve, Con_PriorExp, a_schema)
      learning = learning + Exptime
      Con_afterExp = 1/(1+exp(-a_schema*(learning)))
      schemainfo$conN = schemainfo$conN + Beta_N*(Con_afterExp - Con_PriorExp)
      schemainfo$conVar = schemainfo$conVar + Beta_Var*(abs(Con_afterExp - Con_PriorExp) - schemainfo$conVar)
      
      ### update the generic confidence
      Con_PriorExp = rnorm(1, mean = Gcon$conN, sd = Gcon$conVar)
      
      ### calculate the prior learning progress based on a inverse formula 
      learning = mapply(log_inve, Con_PriorExp, a_generic)
      learning = learning + mean(Exptime)
      Con_afterExp = 1/(1+exp(-a_generic*(learning)))
      Gcon$conN = Gcon$conN + Beta_gN*(Con_afterExp - Con_PriorExp)
      Gcon$conVar = Gcon$conVar + Beta_gVar*(abs(Con_afterExp - Con_PriorExp) - Gcon$conVar)
      
      Thischosen = schemainfo %>% 
        group_by(payoff) %>% 
        summarise(chosen = sample(schemaID, size = 1, prob = NULL))
      
      ##### and we then move to the decision phase ######
      decisiontime = 1
      while (decisiontime <= 2){ ## i.e. 2 phase to choose item
        if (clock >= 5500 | Sys.time() - start.time > 240){
          print('Beyond time in decisiontime <= 2!')
          error = 1
          break
        }
        
        ThisPhase = ifelse(decisiontime == 1, (ThisRound*2)-1, ThisRound*2)
        #### again, before the decision making, we sample the schema-based and generic confidence
        Schema_EI = data.frame(schemaID = schemainfo$schemaID,
                               payoff = schemainfo$payoff)
        Schema_EI$Scon = mapply(rnorm, 1, mean = schemainfo$conN, sd = schemainfo$conVar)
        Schema_EI$Gcon = rnorm(1, mean = Gcon$conN, sd = Gcon$conVar)
        
        #### firstly, we calculate the DMs for each schema
        Schema_EI$weightCon = w*Schema_EI$Scon + (1-w)*Schema_EI$Gcon
        #Schema_EI$DM = mapply(Bonus,Schema_EI$weightCon) * Schema_EI$payoff
        Schema_EI$DM = Schema_EI$weightCon * Schema_EI$payoff # Change:remove the bonus of schema matching in attention component
        
        #### we then determine which schemas will be presented in this screen
        if (decisiontime == 1){
          ChoSchema = Schema_EI[Schema_EI$schemaID %in% Thischosen$chosen,]
        }else if(decisiontime == 2){
          ChoSchema = Schema_EI[!Schema_EI$schemaID %in% Thischosen$chosen,]
        }
        
        ChoSchema$evidence = 0
        ChoItem = Decitem[Decitem$SchemaID %in% ChoSchema$schemaID,] %>% 
          group_by(SchemaID) %>%
          summarise(chosen = sample(ItemID, size = 4, prob = NULL))
        
        #### firstly we prepare a table for evidence integration
        Item_EI = data.frame(ID = Decitem[Decitem$ItemID %in% ChoItem$chosen,1], 
                             Schema = Decitem[Decitem$ItemID %in% ChoItem$chosen,2], 
                             payoff = Decitem[Decitem$ItemID %in% ChoItem$chosen,3])
        Item_EI = Item_EI %>% 
          group_by(Schema) %>% 
          mutate(N = schemainfo$conN[schemainfo$schemaID == Schema], 
                 Var = schemainfo$conVar[schemainfo$schemaID == Schema],
                 DM = ChoSchema$DM[Schema == ChoSchema$schemaID])
        
        Item_EI$evidence = 0 # evidence integration
        Item_EI$recovery = 0
        Item_EI$timevar = 1
        Item_EI$decision = 0
        Item_EI$OB = 0
        Item_EI$AS = 0
        
        # mark the status of item's threshold, 0:inter; 1:final
        Item_EI$status = 0 
        Item_EI$thres = thres_item_inter
        
        #### let's start the evidence integration ####
        schema_decision = 0
        Finish = 0
        
        ## use to get the dwell choice, as Finish changes is faster
        dwell_choice_flag = 0 
        attention = 0
        shift = 0
        CTime = 0
        dwelltime = 0
        
        # This need to be assign every time otherwise it will decay
        thres_schema = Param.df$thres_schema[Subject] 
        while (Finish <= 4){ ## i.e. we start to choose 4 item
          
          if (clock >= 5500){
            print('Beyond time in Finish <= 4!')
            error = 1
            break
          }
          
          if (attention == 0) { ### i.e., when decision has not been made and no item were attended
            Item_EI$timevar = 1-(1/(exp(timevar*Item_EI$recovery)))
            if (sum(Item_EI$decision == 0) == 1){ 
              attention = Item_EI$ID[Item_EI$decision == 0]
            }else{ ## otherwise, use prob formula to choose attention
              if (schema_decision == 0){
                
                # ####YIFEI SHANG: to prevent schema not chosen after all items decided
                # if(mean(Item_EI$decision) == 1){
                #   print("schema not chosen after all items decided")
                #   error = 1
                #   break
                # }
                # #### change:disable error break if not choosing schema decision
                
                p.list = (exp(Phi*Item_EI$evidence[Item_EI$decision == 0] * Item_EI$DM[Item_EI$decision == 0] * Item_EI$timevar[Item_EI$decision == 0]))/
                  sum((exp(Phi*Item_EI$evidence[Item_EI$decision == 0] *Item_EI$DM[Item_EI$decision == 0] * Item_EI$timevar[Item_EI$decision == 0])))
                
                p.list[is.na(p.list)] <- 1
                attention = sample(Item_EI$ID[Item_EI$decision == 0], 1,
                                   prob = p.list)
              } else{
                # p.list <- (exp(Phi*Item_EI$evidence[Item_EI$decision == 0]* Item_EI$timevar[Item_EI$decision == 0]))/
                #   sum((exp(Phi*Item_EI$evidence[Item_EI$decision == 0]* Item_EI$timevar[Item_EI$decision == 0])))
                # p.list[is.na(p.list)] <- 1
                # attention = sample(Item_EI$ID[Item_EI$decision == 0],1,
                #                    prob = p.list)
                #change:the same with the p.list before schema decision
                p.list = (exp(Phi*Item_EI$evidence[Item_EI$decision == 0] * Item_EI$DM[Item_EI$decision == 0] * Item_EI$timevar[Item_EI$decision == 0]))/
                  sum((exp(Phi*Item_EI$evidence[Item_EI$decision == 0] *Item_EI$DM[Item_EI$decision == 0] * Item_EI$timevar[Item_EI$decision == 0])))
                
                p.list[is.na(p.list)] <- 1
                attention = sample(Item_EI$ID[Item_EI$decision == 0], 1,
                                   prob = p.list)
              }
            }
            
            Item_EI$OB[Item_EI$ID == attention] = 1
            Item_EI$AS[Item_EI$ID == attention] = Item_EI$AS[Item_EI$ID == attention] + 1
            shift = 0
          } ## end of attention assign
          
          while (shift == 0) { ### i.e., when the attention does not shift
            
            if (clock >= 5500){
              print('Beyond time in shift == 0!')
              error = 1
              break
            }
            
            ### evidence integration
            CTime = CTime + modeltimestep
            clock = clock + modeltimestep
            dwelltime = dwelltime + modeltimestep
            
            # evidence + infor = new evidence
            Item_EI$evidence[Item_EI$ID == attention] = Item_EI$evidence[Item_EI$ID == attention] +
              rnorm(1, Item_EI$N[Item_EI$ID == attention], Item_EI$Var[Item_EI$ID == attention])
            
            Item_EI$recovery[Item_EI$ID != attention] = Item_EI$recovery[Item_EI$ID != attention] + 1
            
            ### evidence decay
            # evidence decay as time passes
            Item_EI$evidence[Item_EI$ID != attention & Item_EI$decision == 0] = 
              Item_EI$evidence[Item_EI$ID != attention & Item_EI$decision == 0] * decay_speed
            
            # threshold decay as time passes
            Item_EI$thres = Item_EI$thres * decay_speed_thres
            thres_schema = thres_schema * decay_speed_thres
            Item_EI$evidence[Item_EI$evidence > Item_EI$thres] = Item_EI$thres
            
            ### identification completed, once the threshold is reached
            if (Item_EI$evidence[Item_EI$decision == 0 & 
                                 Item_EI$ID == attention ] >= 
                Item_EI$thres[Item_EI$decision == 0 & Item_EI$ID == attention]){
              shift = 1 # attention shift, shift due to item reach threshold
              
              if (#schema_decision != 0 & #change: allow choose item without deciding schema
                  any(Item_EI$evidence[Item_EI$decision == 0 & 
                                       Item_EI$ID == attention & 
                                       Item_EI$status == 1] >= 
                      Item_EI$thres[Item_EI$decision == 0 & 
                                    Item_EI$ID == attention & 
                                    Item_EI$status == 1])){ 
                ## i.e. schema has been chosen, choose item
                ## notice: only the status is 1, 
                ## only in final threshold can item identification be done
                
                
                Outputs_cho[ThisPhase, paste0("Cho_", Finish)] = Item_EI$ID[Item_EI$evidence >= Item_EI$thres &
                                                                              Item_EI$decision == 0 & 
                                                                              Item_EI$ID == attention & 
                                                                              Item_EI$status == 1]
                Outputs_cho[ThisPhase, paste0("RT_", Finish)] = CTime

                Outputs_cho[ThisPhase, paste0("OB_", Finish)] = sum(Item_EI$OB == 1)
                
                # Attention shift
                Outputs_cho[ThisPhase, paste0("AS_", Finish)] = sum(Item_EI$AS)
                
                # Reset
                Item_EI$OB = 0
                Item_EI$AS = 0
                CTime = 0
                Finish = Finish + 1
                dwell_choice_flag = 1 # Finish changes, flag to 1
              }  ## end of item chosen
              
              Item_EI$decision[Item_EI$evidence >= Item_EI$thres & Item_EI$decision == 0 & Item_EI$status == 1] = 1 # mark the decision as 1
              Item_EI$thres[Item_EI$evidence >= Item_EI$thres & Item_EI$decision == 0 & Item_EI$status == 0] = thres_item_final # change the item thres from inter to final one
              Item_EI$status[Item_EI$thres == thres_item_final & Item_EI$decision == 0 & Item_EI$status == 0] = 1
            }else{
              shift = sample(c(1, rep(0, theta_shift)),1, prob = NULL) # attention might shift randomly
            } ## end of item identification check
            
            ###change: disable the whole schema decision mechanism
            
            # if (schema_decision == 0){
            #   ## compute the evidence for each schema
            #   ChoSchema = ChoSchema %>% group_by(schemaID) %>% mutate(evidence = sum(Item_EI$evidence[Item_EI$Schema == schemaID]))
            #   if (any(ChoSchema$evidence >= thres_schema)){  ## i.e. schema identified
            #     if (length(ChoSchema$schemaID[ChoSchema$evidence >= thres_schema]) != 1){
            #       schema_decision = ChoSchema$schemaID[ChoSchema$evidence == max(ChoSchema$evidence)][1]
            #     }else{
            #       schema_decision = ChoSchema$schemaID[ChoSchema$evidence >= thres_schema]
            #     }
            #     
            #     # ## new: turn unchosen schema evidence to 0
            #     # if(model.version == 1){ # version 1: reset evidence to 0
            #     #   Item_EI$evidence[Item_EI$Schema != schema_decision] = 0
            #     #   Item_EI$N[Item_EI$Schema != schema_decision] = 1 - Item_EI$N[Item_EI$Schema != schema_decision]
            #     # }else if(model.version == 2){ # version 2: reset evidence to minus
            #     #   Item_EI$evidence[Item_EI$Schema != schema_decision] = - Item_EI$evidence[Item_EI$Schema != schema_decision] 
            #     # }else if(model.version == 3){ # version 3: reset confidence to minus
            #     #   Item_EI$N[Item_EI$Schema != schema_decision] = - Item_EI$N[Item_EI$Schema != schema_decision]
            #     # }
            #     
            #     
            #     # thres_item - Item_EI$evidence[Item_EI$Schema != schema_decision]
            #     ## rndb confidence is 1 minused the original value
            #     
            #     
            #     
            #     Outputs_cho$Schema[ThisPhase] = schema_decision
            #     Outputs_cho$Schema_RT[ThisPhase] = CTime
            #     Outputs_cho$Schema_OB[ThisPhase] = sum(Item_EI$OB == 1)
            #     Outputs_cho$Schema_AS[ThisPhase] = sum(Item_EI$AS)
            #     Outputs_cho$schema_payoff[ThisPhase] = ChoSchema$payoff[ChoSchema$schemaID == schema_decision]
            #     
            #     # reset measures
            #     # Item_EI$OB = 0
            #     # Item_EI$AS = 0
            #     # CTime = 0
            #     # shift = 1 ## shift due to schema chosen
            #     # Item_EI$decision = 0
            #     # Finish = 1
            #     dwell_choice_flag = 1 ## Finish changes, flag to 1
            #     Item_EI$thres[Item_EI$status == 0] = thres_item_final # turn all item thres to final
            #     Item_EI$status = 1 # new, now all items threshold is final one
            #   }
            # } ## end of schema identification check
            
            if (shift == 1){
              ## new whenever attention shift, we record dwell time 
              ## sometimes Finish has changed one step faster
              ## which_choice is 0:schema chosen; 1-4:4 items chosen
              onerecord = data.frame(Subject=Subject,Round=ThisRound,Phase=ThisPhase,
                                     item_ID=attention,item_Schema=Item_EI$Schema[Item_EI$ID==attention],
                                     which_choice=Finish - dwell_choice_flag, 
                                     dwelltime=dwelltime,afterbreak=break_happens) 
              Outputs_dwell = rbind(Outputs_dwell,onerecord) 
              dwelltime = 0 ## reset dwell time once attention shifts
              dwell_choice_flag = 0 ## reset flag to 0, wait for next Finish change
              
              # once the attention shift, we mark the last item
              Item_EI$recovery[Item_EI$ID == attention] = 0
              Item_EI$timevar[Item_EI$ID == attention] = 0
              attention = 0
            }
          } ## end of evidence integration
          
        }## end of 4 item selection
        
        
        ##### deliver the feedback
        Schema_res = Item_EI$Schema[Item_EI$ID %in% Outputs_cho[ThisPhase,c("Cho_1","Cho_2","Cho_3","Cho_4")]]
        feedback = type(risk, Schema_res, Item_EI, Outputs_cho, ThisPhase, schemainfo)
        Outputs_cho$AC[ThisPhase] = feedback[[1]]
        Outputs_cho$performance[ThisPhase] = feedback[[2]]
        
        ###########0717 changed Yifei##############
        Outputs_cho$payoff[ThisPhase] = feedback[[3]]
        ###########0717 changed Yifei##############
        
        ## new mark the after break
        Outputs_cho$afterbreak[ThisPhase] = break_happens
        
        Outputs_learn = rbind(Outputs_learn,schemainfo)
        Outputs_glearn = rbind(Outputs_glearn,Gcon)
        
        # change:make confidence update based on their payoff,not accuracy
        schemainfo$conN[schemainfo$schemaID %in% unique(Schema_res)] = 
          schemainfo$conN[schemainfo$schemaID %in% unique(Schema_res)] + 
          Beta_N*(Outputs_cho$payoff[ThisPhase]/6 - Schema_EI$Scon[Schema_EI$schemaID %in% unique(Schema_res)])
        
        schemainfo$conVar[schemainfo$schemaID %in% unique(Schema_res)] = 
          schemainfo$conVar[schemainfo$schemaID %in% unique(Schema_res)] + 
          Beta_Var*(abs(Schema_EI$Scon[Schema_EI$schemaID %in% unique(Schema_res)] - Outputs_cho$payoff[ThisPhase]/6) - 
                      schemainfo$conVar[schemainfo$schemaID %in% unique(Schema_res)])
        
        Gcon$conN = Gcon$conN + Beta_gN*(Outputs_cho$payoff[ThisPhase]/6 - unique(Schema_EI$Gcon))
        Gcon$conVar = Gcon$conVar + Beta_gVar*(abs(Outputs_cho$payoff[ThisPhase]/6 - unique(Schema_EI$Gcon)) - Gcon$conVar)
        decisiontime = decisiontime + 1
      } ## end of phase item choose
      ThisRound = ThisRound + 1
    } ## end of simulation for 1 subject
    
    if (error != 1){
      Outputs_cho$breakR = breakR ## new add break round
      ## before rbind, we delete redundant rows (i.e. unfinished choice)
      Outputs_cho = Outputs_cho[Outputs_cho$RT_4!=0, ]
      Outputs_dwell = Outputs_dwell[Outputs_dwell$Phase<=max(Outputs_cho$Phase),]
      ALL.df <- rbind(ALL.df, Outputs_cho)
      dwelltime.df <- rbind(dwelltime.df, Outputs_dwell)
      
      ## record the learn time, there are 3 time it is update per round
      ## 1. exploration; 2. 2 phases based on feedback
      
      Time = rep(1:((ThisRound-1)*3),each=10)
      Outputs_learn <- cbind(Outputs_learn, Time)
      Outputs_learn$Subject = Subject
      Outputs_glearn$Subject = Subject 
      if(
        #sim.mode=="before" | 
        save.confi == T){
        confidence.df <- rbind(confidence.df, Outputs_learn)
      }
      gconfidence.df <- rbind(gconfidence.df, Outputs_glearn)
    }
    
    ####### GARBAGE COLLECTOR
    gc()
    
  } ## end of subjectnum of subjects simulation
  
  
  ## result process: generating allresult df
  ## include type information
  ALL.df$type = type
  ## get the mean of dwell time per choice
  dwell_mean <- dwelltime.df %>%
    mutate(choice = replace(which_choice, which_choice == 0, 1)) %>%
    group_by(Subject, Round, Phase, choice, afterbreak) %>%
    summarise(dwell_mean=mean(dwelltime)) %>%
    pivot_wider(names_from = choice,
                names_glue = 'dwellmean_{choice}',
                values_from = dwell_mean)
  ## add the dwell time to other measures
  allresult <- merge(ALL.df,dwell_mean,by=c("Subject",'Round',"Phase","afterbreak")) # with dwell time on it
  allresult <- allresult %>% arrange(Subject,Phase)
  ## in experiments, we cannot distinguish between schema and item 1 choice
  ## so we add them up here in the model
  allresult <- allresult %>%
    mutate(RT_1 = Schema_RT + RT_1,
           OB_1 = Schema_OB + OB_1,
           AS_1 = Schema_AS + AS_1)
  ## ob can larger than 20 due to addition, but ob can only be a maximum of 20
  allresult$OB_1[allresult$OB_1 > 20] <- 20
  
  
  ## now save results
  if(save){
    if (!dir.exists(simupath)){
      dir.create(simupath, recursive = T)
    }
    write_csv(allresult,file.path(simupath,'allresult_processed.csv'))
    write_csv(Param.df, file.path(simupath,'Paras.csv'))
    
    if(
      #sim.mode == "before" | 
      save.confi == T){
      write_csv(confidence.df, file.path(simupath,'confidence.csv'))
      write_csv(gconfidence.df, file.path(simupath,'gconfidence.csv'))
    }
  }
  
  ## return data res
  return(list(param=Param.df,
              allresult_processed=allresult))
}


typelist = c("H","Hc","HL","L","Lc","LH")
exp_typelist = c("painting", "quote")
subjectnum = 100
Param.df <- data.frame(Subject = 1:subjectnum,
                       a_schema = rep(0.2,subjectnum),
                       h_schema = rep(1000,subjectnum),
                       Beta_N=rep(0.2,subjectnum),
                       Beta_Var= rep(0.3,subjectnum),
                       a_generic  = rep(0.1,subjectnum),
                       h_generic = rep(1500,subjectnum),
                       Beta_gN = rep(0.1,subjectnum),
                       Beta_gVar = rep(0.2,subjectnum),
                       w = rep(0.3,subjectnum),
                       Phi = rep(20,subjectnum),
                       decay_speed = rep(0.999,subjectnum),
                       decay_speed_thres = rep(0.999,subjectnum),
                       thres_item_inter  = rep(6,subjectnum),
                       thres_item_final = rep(13.75,subjectnum),
                       thres_schema = rep(50,subjectnum),
                       theta_shift = rep(3, subjectnum),
                       timevar = rep(0.0001,subjectnum),
                       modeltimestep = rep(0.061 ,subjectnum))

res <- simulation(Param.df, "L", "painting", save = T,
                  savepath = "res100_simulation_modified/tmp", sim.mode="before",
                  before.path = NA,save.confi = T,
                  scale.confi.init = T)

res <- simulation(Param.df, "L", "painting", save = T,
                  savepath = "res100_simulation_modified/tmp/after_tmp", sim.mode="after",
                  before.path = file.path("res100_simulation_modified/tmp", "L"),
                  scale.confi.init = T)

# valid parameter ranges
init.range.fixed.0804 <- data.frame(
  Beta_N = c(0.001, 0.7),
  Beta_gN = c(0.001, 1),
  w = c(0.1, 0.9),
  Phi = c(1, 50),
  decay_speed = c(0.99, 0.9995),
  decay_speed_thres = c(0.99, 0.9995),
  thres_item_final = c(7.8, 50),
  theta_shift = c(1, 10),
  thres_schema = c(7.8, 50),
  
  a_schema = c(0.01, 1),
  h_schema = c(10, 3000),
  Beta_Var = c(0.001, 0.4),
  a_generic = c(0.01, 1),
  h_generic = c(10, 3000),
  Beta_gVar = c(0.001, 1),
  thres_item_inter = c(0.01, 7.75),
  timevar = c(0.0000001, 0.002),
  modeltimestep = c(0.001, 0.1)
)