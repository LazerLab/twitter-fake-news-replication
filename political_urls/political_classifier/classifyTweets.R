library(data.table)
library(text2vec)   # faster alternative to tm
library(tokenizers)  # word tokenizing
library(glmnet)
library(ROCR)
source("textProcessingForClassifier.R")


# Uses glmnet (by default) or naive Bayes (faster, and almost as accurate)

# Decisions made:
# -Defining positives and negatives for training.
#   Positives: any tweet that matches whitelist.
#   Negatives: random tweets. 
#   When applying classifier, it's fast/simple to get predictions for all tweets. Labeled positives: always set their final scores to 1.
# -How to set a threshold on classifier's predictions?
#   Always use equal sized classes at training time, then choose the cutoff later. 
#   (Experiments showed .5 is too low, .8 is reasonable, and .95 is good for high quality.)
# -Features used in classifier: 
#   Important to remove whitelist terms, otherwise the problem is too easy and the classifier overfits.
#   How? --> Remove terms from the tweets during the filtering step, as I did in earlier code. This beats alt options, because 
#       it's important to remove the whole term (or URL), not just the part that matches a whitelist term.
#       (Alt option:  Keep track of what the whitelist terms will look like after stemming by constructing a pseudo-doc 
#       that contains just them and seeing what columns of the DocTermMatrix they map to. Then remove these cols of DTM. 
#       (Alt option: use "exclude" flag to glmnet, but would require passing the col indices around.)
# -Tokenizing: Do want to treat hashtags, handles and URLs differently from regular text. 
#    Both at whitelist time, and when tokenizing into docTermMatrix.



# Expects input data.table to have a column called complete_raw_text.
# Returns a data.table with original columns plus: classifier_label, white_terms, rest_of_text,
#    raw_classifier_score, political_classifier_score
classifyTweets = function(keywordFile, tweetData, modelFileOutStem, startTime, goFastUseNaiveBayes=F, noTraining=F, loadVectorizer=F) {

    whitelistTerms = read.table(keywordFile, sep="\n", comment.char = "", blank.lines.skip = T, 
                                na.strings = NULL, quote='', colClasses="character")[[1]]
        
    tweetDataMore = checkForWhitelistTerms(whitelistTerms, tweetData$complete_raw_text, removeWLTerms=T)
    print(paste("Done checking for whitelist terms at", Sys.time() - startTime))
    tweetData = cbind(tweetData, tweetDataMore)  # new columns: classifier_label, white_terms, rest_of_text
    docTermMatrix = tokenizeToDocTermMatrix(tweetData$rest_of_text, modelFileOutStem, loadVectorizer=loadVectorizer)
    print(paste("Done constructing docTermMatrix at", Sys.time() - startTime))
    print(paste("matrix dims:", nrow(docTermMatrix), "x", ncol(docTermMatrix)))
    
    # 2. Train/load classier, then apply to new data
    if (goFastUseNaiveBayes) {
        if (noTraining & !is.null(modelFileOutStem)) {
            model <- readRDS(paste0(modelFileOutStem, "-nb.rds"))
            print(paste("Done loading Naive Bayes classifier at", Sys.time() - startTime))
        } else {
            model <- trainNaiveBayesClassifier(docTermMatrix, tweetData$classifier_label, modelFileOutStem)
            print(paste("Done training Naive Bayes classifier at", Sys.time() - startTime))
        }
        classifierScores = naiveBayes.predict(model, docTermMatrix)
    } else {
        if (noTraining & !is.null(modelFileOutStem)) {
            model <- readRDS(paste0(modelFileOutStem, "-glm.rds"))
            print(paste("Done loading glmnet classifier at", Sys.time() - startTime))
        } else {
            model <- trainLogRegClassifier(docTermMatrix, tweetData$classifier_label, modelFileOutStem)
            print(paste("Done training glmnet classifier at", Sys.time() - startTime))
        }
        classifierScores = glmnet.predict(model, docTermMatrix)
    }
    
    tweetData[, c("raw_classifier_score", "political_classifier_score") := classifierScores]   # one vector of preds --> 2 new cols
    print(paste("Done applying classifier to data at", Sys.time() - startTime))
    
    # rawClassifierScore has predictions for whitelisted tweets. For output, change these to 1.
    tweetData[classifier_label==T, political_classifier_score := 1]
    
    return(tweetData)

}


# Fits a logistic regression 
# docTermMatrix: matrix of items x features
# classifier_labels: logical vector, one entry per item
# Returns a vector of classifier predictions in the same order as the input data
trainLogRegClassifier = function(docTermMatrix, classifier_labels, modelFileOutStem) {
    
    # 1. Choose positives and negatives for training set
    #   Use all the positives and sample an equal number of negatives.
    numPositives = sum(classifier_labels)
    print(paste("Found", numPositives, "positives"))
    posRowIDs = which(classifier_labels)
    if (numPositives >= .2 * length(classifier_labels)) {  
        # decided not to worry about trainingSet size or proportion, but at least give a yell if it's bigger than expected
        warning("Gee, that's a lot of positives")
    }
    numNegatives = min(length(classifier_labels) - numPositives, numPositives)
    print(paste("Sampling", numNegatives, "negatives to use as training data"))
    negRowIDs = sample(which(!classifier_labels), numNegatives)  
    trainData = rbind(docTermMatrix[posRowIDs,], docTermMatrix[negRowIDs,])
    trainLabels = c(rep(T, numPositives), rep(F, numNegatives))
    
    
    # 2. Build classifier and save it
    # Default setting gives (alpha = 1) == L1 norm == LASSO
    
    print("Calling glmnet with cross-validation")
    cvfit = cv.glmnet(trainData, trainLabels, family="binomial", keep=T, type.measure="class")  # defaults to nfolds=10 and optimizing type.measure="deviance"
    
    if (!is.null(modelFileOutStem)) {
        outfile = paste0(modelFileOutStem, "-glm.rds")
        print(paste("Saving fitted model to file", outfile))
        saveRDS(cvfit, file = outfile)
    }
    
    # Report performance stats from cross-val run. 
    # (Purpose of CV is to choose lambda. Once that's done, we can look at each item's (hold-out) prediction with that lambda; 
    # it's using knowledge of this data, but isn't overfit as much as if we looked at preds from full model below.)
    whichLambdaIndex = which(cvfit$lambda == cvfit$lambda.min)  # safe wrt floating point math? apparently ok.
    print(paste("Misclassification rate (from cross-val for lambda) on training data:", cvfit$cvm[whichLambdaIndex]))
    # cvfit does contain a prediction for each item at each lambda.
    cvpreds = cvfit$fit.preval[,whichLambdaIndex]
    printConfusionMatrix(cvpreds, trainLabels, .5)
    predObj = prediction(cvpreds, trainLabels)
    print(paste("AUC:", performance(predObj, "auc")@y.values[[1]]))
    
    sanityCheckModelCoeffs(cvfit)
    if (!is.null(modelFileOutStem)) {
        plotTrainingDistributions(cvpreds, numPositives, modelFileOutStem)
        save(posRowIDs, negRowIDs, file=paste0(modelFileOutStem, "-trainIDs.Rdata"))
    }

    return(cvfit)
}

glmnet.predict = function(model, new_data) {
    return(predict(model, newx = new_data, type="response", s="lambda.min"))
}

printConfusionMatrix = function(votes, truth, cutoff) {
    # (simpler way would be using table())
    print("                FALSE   TRUE (predicted)")
    print(paste("(truth) FALSE", sum(votes < cutoff & truth == "FALSE"), sum(votes >= cutoff & truth == "FALSE"),  collapse="\t"))
    print(paste("        TRUE", sum(votes < cutoff & truth == "TRUE"), sum(votes >= cutoff & truth == "TRUE"),  collapse="\t"))
}

sanityCheckModelCoeffs = function(cvfit) {
    t1 = coef(cvfit, s = "lambda.min")
    t2 = sort(t1[which(t1 != 0),])
    print(paste("looking at coefficients:", length(t2), "are non-zero"))
    print("most positive:")
    print(tail(t2, 20))
    print("most negative:")
    print(head(t2, 20))
    
    candidates = c("clinton", "hillari", "hillary", "trump", "donald")
    print("candidates:")
    print(t2[which(names(t2) %in% candidates)])
}
plotTrainingDistributions = function(cvpreds, numPositives, modelFileOutStem) {
    pdf(paste0(modelFileOutStem, "-trainPreds.pdf"))
    plot(density(cvpreds[(numPositives+1):length(cvpreds)]), col="blue")
    lines(density(cvpreds[1:numPositives]), col="red")
    dev.off()
}

# Much simpler and faster than logistic regression, and almost as accurate.
trainNaiveBayesClassifier = function(docTermMatrix, classifier_labels, modelFileOutStem, withCrossVal=T) {
    
    # 0. Make features binary (not counts)
    dtmBinary = docTermMatrix
    dtmBinary@x[dtmBinary@x > 1] = 1
    
    # 1. Choose positives and negatives for training set
    #   Use all the positives and sample an equal number of negatives.
    # Compared to glmnet, could choose to oversample negatives (to learn that class better), then use the prior
    # to set the class balance back to 50-50 (which we'd want mainly for compatibility with glmnet's prediction scores).
    numPositives = sum(classifier_labels)
    print(paste("Found", numPositives, "positives"))
    posRowIDs = which(classifier_labels)
    if (numPositives >= .2 * length(classifier_labels)) {  
        # decided not to worry about trainingSet size or proportion, but at least give a yell if it's bigger than expected
        warning("Gee, that's a lot of positives")
    }
    numNegatives = min(length(classifier_labels) - numPositives, numPositives)
    print(paste("Sampling", numNegatives, "negatives to use as training data"))
    negRowIDs = sample(which(!classifier_labels), numNegatives)  
    trainData = rbind(dtmBinary[posRowIDs,], dtmBinary[negRowIDs,])
    trainLabels = c(rep(T, numPositives), rep(F, numNegatives))
    
    if (withCrossVal) {
        print("Running cross-val to get accuracy estimates")
        # Need to handle cross-validation explicitly to get hold-out scores that are comparable to glmnet.
        nfold = 10
        foldAssignments = sample(nfold, nrow(trainData), replace = T)
        allFoldPreds = vector(mode="numeric", length=nrow(trainData))

        for (i in 1:nfold) {
            foldTrainRows = trainData[foldAssignments != i,]
            foldTestRows = trainData[foldAssignments == i,]
            foldTrainLabels = trainLabels[foldAssignments != i]
            #foldTestLabels = trainLabels[foldAssignments == i]
        
            foldPreds = binaryNBModel(foldTrainRows, foldTrainLabels, foldTestRows)
            allFoldPreds[foldAssignments == i] = foldPreds
        }
        
        # How did we do on labeled data?
        printConfusionMatrix(allFoldPreds, trainLabels, .5)
        predObj = prediction(allFoldPreds, trainLabels)
        print(paste("AUC:", performance(predObj, "auc")@y.values[[1]]))
        
        if (!is.null(modelFileOutStem)) {
            plotTrainingDistributions(allFoldPreds, numPositives, modelFileOutStem)
            save(posRowIDs, negRowIDs, file=paste0(modelFileOutStem, "-trainIDs.Rdata"))
        }
    }
    
    model = binaryNBModel(trainData, trainLabels, dtmBinary, modelFileOutStem, printModelCoeffs=T)
    return(model)     
}

# simplest little model that takes binary features and a binary label, outputs vector of predicted probabilities P(pos | X) for testData
binaryNBModel = function(trainData, trainLabels, testData, modelFileOutStem, laplace=1, printModelCoeffs=F) {
    # recreating vars that were in calling function
    posRowIDs = which(trainLabels == T)
    negRowIDs = which(trainLabels == F)
    numPositives = length(posRowIDs)
    numNegatives = length(negRowIDs)
    
    priorProbPos <- numPositives / (numPositives + numNegatives)
    dtmPos = trainData[posRowIDs,]
    dtmNeg = trainData[negRowIDs,]
    
    # Learn P(x | pos) and P(x | neg)
    countXPos = colSums(dtmPos) + laplace
    probXPos = countXPos / (numPositives + 2 * laplace)
    probNotXPos = 1 - probXPos
    countXNeg = colSums(dtmNeg) + laplace
    probXNeg = countXNeg / (numNegatives + 2 * laplace)
    probNotXNeg = 1 - probXNeg
    
    # Precompute terms to use in making predictions
    # if it were all 0's:
    default_logPred = log(priorProbPos / (1 - priorProbPos)) + sum(log(probNotXPos / probNotXNeg))
    # every time there's a 1, need to (a) add log(probXPos/probXNeg), and (b) subtract the unnecessary log(probNotXPos / probNotXNeg)
    termsToAddWhenX = log(probXPos/probXNeg) - log(probNotXPos / probNotXNeg)
    
    if (printModelCoeffs) {
        sanityCheckNBModelCoeffs(termsToAddWhenX)
    }
    model <- list(llr=termsToAddWhenX, llr_offset=default_logPred)
    if (!is.null(modelFileOutStem)) {
        outfile = paste0(modelFileOutStem, "-nb.rds")
        print(paste("Saving NB fitted model to file", outfile))
        saveRDS(model, file = outfile)
    }
    return(model)
}

naiveBayes.predict <- function(model, new_data) {   
    dtmBinary = new_data
    dtmBinary@x[new_data@x > 1] = 1
    # Make predictions for all items at once, using a matrix multiplication:
    llr = (dtmBinary %*% model$llr) + model$llr_offset
    probs = (exp(llr)/(1 + exp(llr)))[,1]
    return(probs)
    
}

sanityCheckNBModelCoeffs = function(LLRtermsToAddWhenX) {
    t2 = sort(LLRtermsToAddWhenX[which(LLRtermsToAddWhenX != 0)])
    print(paste("looking at LLR terms:", length(t2), "are non-zero"))
    print("most positive:")
    print(tail(t2, 20))
    print("most negative:")
    print(head(t2, 20))
    
    candidates = c("clinton", "hillari", "hillary", "trump", "donald")
    print("candidates:")
    print(t2[which(names(t2) %in% candidates)])
}
    
