package edu.berkeley.nlp.entity.wiki

import edu.berkeley.nlp.entity.joint.LikelihoodAndGradientComputer
import edu.berkeley.nlp.entity.joint.GeneralTrainer
import edu.berkeley.nlp.futile.LightRunner
import edu.berkeley.nlp.futile.math.SloppyMath
import scala.collection.mutable.ArrayBuffer
import edu.berkeley.nlp.futile.fig.basic.Indexer
import edu.berkeley.nlp.futile.util.Counter
import edu.berkeley.nlp.futile.util.Logger
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.JavaConverters._
import edu.berkeley.nlp.entity.GUtil
import edu.berkeley.nlp.entity.coref.CorefDoc
import edu.berkeley.nlp.entity.lang.Language
import edu.berkeley.nlp.entity.ConllDocReader
import edu.berkeley.nlp.entity.coref.CorefDocAssembler
import edu.berkeley.nlp.entity.coref.MentionPropertyComputer

import scala.collection.mutable

case class QueryChoiceExample(val queries: Seq[Query],
                              val denotations: Seq[String],
                              val correctQueryIndices: Array[Int]) {
  var cachedFeatsEachQuery: Array[Array[Int]] = null;
}

class QueryChoiceComputer(val wikiDB: WikipediaInterface,
                          val featureIndexer: Indexer[String]) extends LikelihoodAndGradientComputer[QueryChoiceExample] {
  
  def addFeat(feat: String, feats: ArrayBuffer[Int], addToIndexer: Boolean) {
    val idx = if (addToIndexer) {
      featureIndexer.getIndex(feat);
    } else {
      featureIndexer.indexOf(feat);
    }
    if (idx != -1) feats += idx;
  }
  
  def featurizeUseCache(ex: QueryChoiceExample, addToIndexer: Boolean) = {
    if (ex.cachedFeatsEachQuery == null) {
      ex.cachedFeatsEachQuery = featurize(ex, addToIndexer);
    }
    ex.cachedFeatsEachQuery;
  }
  
  //////////////////////
  // FEATURIZING CODE //
  //////////////////////
  
  def binSize(size: Int) = if (size > 3) ">3" else "" + size;
  
  def approximatelyMatches(str1: String, str2: String) = str1.trim
  
  def featurize(ex: QueryChoiceExample, addToIndexer: Boolean) = {
    if (ex.cachedFeatsEachQuery == null) {
      val queries = ex.queries;
      val queriesNonempty = queries.map(query => wikiDB.disambiguateBestNoDisambig(query) != NilToken);
      val queryIndicesNonempty = queriesNonempty.zipWithIndex.filter(_._1).map(_._2);
      val ment = queries.head.originalMent;
      val mentUpToHeadSize = ment.headIdx - ment.startIdx + 1;
      val firstNonemptyIdx = if (queryIndicesNonempty.isEmpty) -1 else queryIndicesNonempty(0);
      // Extract features that don't depend on denotations
      val queryFeatures = featurizeQueries(queries, addToIndexer);
      // Now extract features for one-best denotations
      ex.cachedFeatsEachQuery = (0 until ex.queries.size).map(i => {
        val feats = new ArrayBuffer[Int];
        feats ++= queryFeatures(i);
        val isNonempty = queriesNonempty(i);
        val query = queries(i);
        val queryType = query.queryType;
        val properDescriptor = if (ment.pos(ment.headIdx - ment.startIdx) == "NNP") "PROP" else "NOM";
        val queryDescriptor = queryType + "-" + isNonempty;
        val queryDescriptorWithProper = properDescriptor + "-" + queryDescriptor;
        def feat(str: String) = addFeat(str, feats, addToIndexer);
        if (isNonempty) { 
          val queryStr = query.getFinalQueryStr;
          val den = ex.denotations(i);
          val denotationHasParenthetical = den.contains("(") && den.endsWith(")");
          
          val matchesQuery = den.toLowerCase == queryStr.toLowerCase;
          feat("MatchesQuery=" + queryDescriptorWithProper + "-" + matchesQuery)
          if (!matchesQuery) {
            feat("ContainsQuery=" + queryDescriptorWithProper + "-" + (den.toLowerCase.contains(queryStr.toLowerCase)));
            feat("StartsWithQuery=" + queryDescriptorWithProper + "-" + (den.toLowerCase.startsWith(queryStr.toLowerCase)));
            feat("EndsWithQuery=" + queryDescriptorWithProper + "-" + (den.toLowerCase.endsWith(queryStr.toLowerCase)));
          }
          feat("ContainsParenthetical=" + queryDescriptorWithProper + "-" + denotationHasParenthetical);
          if (denotationHasParenthetical) {
            feat("MatchesQueryUpToParen=" + queryDescriptorWithProper + "-" + (den.substring(0, den.indexOf("(")).trim.toLowerCase == queryStr.toLowerCase))
          }
//          feat("ApproximatelyMatchesQuery=" + queryDescriptorWithProper + "-" + )
//          feat("ApproximatelyMatchesQueryUpToParen=" + queryDescriptorWithProper + "-" + )
        }
        
        feats.toArray;
      }).toArray;
    }
    ex.cachedFeatsEachQuery;
  }
  
  // Features on just queries and surface factors (e.g. they do not depend on denotations)
  def featurizeQueries(queries: Seq[Query], addToIndexer: Boolean): Array[Array[Int]] = {
    val queriesNonempty = queries.map(query => wikiDB.disambiguateBestNoDisambig(query) != NilToken);
    val queryIndicesNonempty = queriesNonempty.zipWithIndex.filter(_._1).map(_._2);
    val ment = queries.head.originalMent;
    val mentUpToHeadSize = ment.headIdx - ment.startIdx + 1;
    val firstNonemptyIdx = if (queryIndicesNonempty.isEmpty) -1 else queryIndicesNonempty(0);
    Array.tabulate(queries.size)(i => {
      val feats = new ArrayBuffer[Int];
      def feat(str: String) = addFeat(str, feats, addToIndexer);
      val query = queries(i);
      val isNonempty = queriesNonempty(i);
      val indices = query.finalSpan;
      val querySize = indices._2 - indices._1;
      val queryType = query.queryType;
      val properDescriptor = if (ment.pos(ment.headIdx - ment.startIdx) == "NNP") "PROP" else "NOM";
      val queryDescriptor = queryType + "-" + isNonempty;
      feat("FirstNonempty=" + (firstNonemptyIdx == i));
      feat("TypeAndNonempty=" + queryType + "-" + isNonempty);
      feat("DescriptorSize=" + queryDescriptor + "-" + properDescriptor + "-" + binSize(querySize));
      val tagsWithin = ment.pos.slice(indices._1, indices._2);
      val contextTag = ment.contextPosOrPlaceholder(indices._1 - 1);
      val longQuery = tagsWithin.size > 3;
      feat("DescriptorQueryTags=" + queryDescriptor + "-" + contextTag + (if (longQuery) "...") + tagsWithin.slice(Math.max(0, tagsWithin.size - 3), tagsWithin.size).toString);
      feat("DescriptorHead=" + queryDescriptor + "-" + binSize(querySize) + "-" + ment.headStringLc);
      for(f <- query.features)
        feat(f)
      feats.toArray;
    });
  }

  def getDentationLinksSets(denotations: Seq[String], wikiDB: WikipediaInterface) : (Seq[Set[Int]], Seq[Set[Int]]) = {
    (denotations.map(wikiDB.linksDB.getInLinksSetUseCache(_)), denotations.map(wikiDB.linksDB.getOutLinksSetUseCache(_)))
  }

  val logsv = (0 until 3000).map(Math.log(_))

  def logs(i: Int) = {
    if(i < logsv.size)
      logsv(i)
    else
      Math.log(i)
  }

  def unionSize[T](ss: Set[T]*) = {
    val ns = new mutable.HashSet[T]()
    for(s <- ss) {
      ns ++= s
    }
    ns.size
  }

  def intersectSize[T](a: Set[T], b: Set[T]) = {
    var smaller: Set[T] = a
    var larger: Set[T] = b
    if(a.size > b.size) {
      larger = a
      smaller = b
    }
    var ret = 0
    for(i <- smaller) {
      if(larger.contains(i))
        ret += 1
    }
    ret
  }

  def NGD[T](a: Set[T], b: Set[T], wsize: Int) : Double = {
    (logs(math.max(a.size, b.size)) - logs(intersectSize(a,b))) /
      (logs(wsize) - logs(math.min(a.size,b.size)))
  }

  def PMI[T](a: Set[T], b: Set[T], wsize: Int) : Double = {
    // TODO: ? the use of wsize here does not make since
    // must be misunderstanding something
    (intersectSize(a,b) * wsize).asInstanceOf[Double] / (a.size * b.size + 1)
  }
/*
  def GLOWfeatures[T](fn: (Set[T], Set[T], Int) => Double, refs: Seq[Set[T]], prefix: String): Seq[Array[String]] = {
    val rsize = refs.size
    val wsize = unionSize(refs:_*)
    var max = Double.NegativeInfinity
    var avg = 0.0
    // TODO: rank the items in the list
    //val valList = new mutable.MutableList[Double]()
    val cache = new mutable.HashMap[Int,Double] {
      override def initialSize: Int = rsize*rsize
    }
    for(a <- 0 until rsize; b <- 0 until rsize) {
      if(a != b) {
        val v = fn(refs(a), refs(b), wsize)
        cache.put(a + b*65536, v)
        if(v > max)
          max = v
        //valList += v
        avg += v
      }
    }
    avg /= (rsize * (rsize - 1))
    for(a <- 0 until rsize) yield {
      var isInMax = false
      var isAboveAvg = false
      var isAboveAvg2 = false
      for(b <- 0 until rsize) {
        if(a != b) {
          //val v = fn(refs(a),refs(b),wsize)
          val v : Double = cache.getOrElse(a + b*65536, 0.0)
          if(v == max) {
            isInMax = true
          }
          if(v > avg) {
            isAboveAvg = true
          }
          if(v > (avg * 2)) {
            isAboveAvg2 = true
          }
        }
      }
      val r = new ArrayBuffer[String]
      if(isInMax)
        r += prefix + "IsInMax"
      if(isAboveAvg)
        r += prefix + "isAboveAvg"
      if(isAboveAvg2)
        r += prefix + "isAboveAvg2"
      r.toArray
    }
  }*/

  def featurizeQueriesAndDenotations_GLOW(queries: Seq[Query], denotations: Seq[String], addToIndexer: Boolean, wikiDB: WikipediaInterface, goldKnowledgeSet: Seq[String]): Array[Array[Array[Int]]] = {
    val queryOutcomes = queries.map(query => wikiDB.disambiguateBestGetAllOptions(query));
    val queryNonemptyList = queryOutcomes.map(_.isEmpty);
    val ment = queries.head.originalMent;
    val mentUpToHeadSize = ment.headIdx - ment.startIdx + 1;
    //val doc = ment.rawDoc

    val (otherGoldLinksIn, otherGoldLinksOut) = getDentationLinksSets(goldKnowledgeSet, wikiDB)

    // TODO: it appears that there are some possible denotations that have no in/out links
    // which must mean that there is an issue with the wikipedia query extracting system
    val (refLinksIn, refLinksOut) = getDentationLinksSets(denotations, wikiDB)

    val totalTitles = otherGoldLinksIn.foldLeft(Set[Int]())(_ | _) | otherGoldLinksOut.foldLeft(Set[Int]())(_ | _) | refLinksIn.foldLeft(Set[Int]())(_ | _) | refLinksOut.foldLeft(Set[Int]())(_ | _)

    val linkInd = denotations.map(d => {
      val id = wikiDB.linksDB.getPageId(d.replace(" ", "_"))
      (otherGoldLinksIn.map(_.contains(id)), otherGoldLinksOut.map(_.contains(id)))
    })

    val pmingdvals = for(den <- 0 until denotations.size) yield {
      val vector = new Array[Double](9) // idk sum this or something
      // TODO: include the max in here somehow
      //val maxv = new Array[Double](9)
      for (other <- 0 until otherGoldLinksIn.size) {
        val ind1 = linkInd(den)._1(other) // point at this
        val ind2 = ind1 && linkInd(den)._2(other) // point at eachother
        if(ind1) {
          val pmiIn = PMI(refLinksIn(den), otherGoldLinksIn(other), totalTitles.size)
          val ngdIn = NGD(refLinksIn(den), otherGoldLinksIn(other), totalTitles.size)
          val pmiOut = PMI(refLinksOut(den), otherGoldLinksOut(other), totalTitles.size)
          val ngdOut = NGD(refLinksOut(den), otherGoldLinksOut(other), totalTitles.size)
          //if(ind1) {
          vector(0) += pmiIn
          vector(1) += ngdIn
          vector(2) += pmiOut
          vector(3) += ngdOut
          //}
          if(ind2) {
            vector(4) += pmiIn
            vector(5) += ngdIn
            vector(6) += pmiOut
            vector(7) += ngdOut
            vector(8) += 1
          }
        } else {
          // then the indicators for this case are false, and we will not be adding anything
          // to the weight vector for this entry.
        }
      }
      for(i <- 0 until vector.size) {
        // Get the average
        vector(i) /= otherGoldLinksIn.size
      }
      vector
    }
    val maxpmingd = new Array[Double](9)
    for(v <- pmingdvals) {
      for(i <- 0 until 9) {
        if(v(i) > maxpmingd(i)) {
          maxpmingd(i) = v(i)
        }
      }
    }

    /*val PMINGDvals = Seq(
      GLOWfeatures[Int](PMI, refLinksIn, "PMI-in-"),
      GLOWfeatures[Int](NGD, refLinksIn, "NGD-in-"),
      GLOWfeatures[Int](PMI, refLinksOut, "PMI-out-"),
      GLOWfeatures[Int](NGD, refLinksOut, "NGD-out-")
    )*/

    // TODO: this is not correct,.....
    // we need to know what we are going to annonate stuff in the document with,
    // these are going to be denotations for a single example, which won't be useful
    // so we need to get all the possible annontations for a given document
    //
    // in the wikification paper they have something that is choosing the references together
    // need to look at pairs of references and

    val denotationSim = denotations.map(t => {
      List(
        wikiDB.textDB.compareDocumentC(ment.rawDoc.documentVectorCache, t),
        wikiDB.textDB.compareDocumentC(ment.rawDoc.contextVectorCache, t),
        wikiDB.textDB.compareContextC(ment.rawDoc.documentVectorCache, t),
        wikiDB.textDB.compareContextC(ment.rawDoc.contextVectorCache, t)
      )
    })
    val denotationSimMax = denotationSim.reduce((a,b) => {
      (0 until 4).map(i => Math.max(a(i), b(i))).toList
    })
    val denotationSimSum: List[Double] = denotationSim.reduce((a,b) => {
      (0 until 4).map(i => a(i) + b(i)).toList
    })
    val denotationSimAvg = (0 until 4).map(i => (denotationSimSum(i) / denotationSim.size))

    // TODO: implement the local vector features which compare the text of the pages
    // the context can be the set of items linking into/outof a page? but then that isn't the similarity



    Array.tabulate(queries.size, denotations.size)((queryIdx, denIdx) => {
      val feats = new ArrayBuffer[Int];
      def feat(str: String) = addFeat(str, feats, addToIndexer);
      def featUpToVal(str: String, vv: Int) = {
        // there is some infty in the value, so dividing by zero or something...
        if(vv == Integer.MAX_VALUE) {
          feat(str + "max-int-val")
        } else {
          for (i <- -1 until vv) {
            feat(str + vv)
          }
        }
      }
      /*for(p <- PMINGDvals)
        for(f <- p(denIdx))
          feat(f)
      */
      val query = queries(queryIdx);
      val den = denotations(denIdx);
      feat("OnEverything");
      if (den == NilToken) {
        feat("NilAndQueryNonempty=" + queryNonemptyList(queryIdx));
      } else if (queryOutcomes(queryIdx).containsKey(den)) {
        val queryDescriptorWithProper = (if (ment.pos(ment.headIdx - ment.startIdx) == "NNP") "PROP" else "NOM") + "-" + query.queryType;
        val queryRank = queryOutcomes(queryIdx).getSortedKeys().indexOf(den);
        feat("Rank=" + queryDescriptorWithProper + "-" + (queryRank + 1))
        val queryStr = query.getFinalQueryStr;
        val matchesQuery = den.toLowerCase == queryStr.toLowerCase;
        feat("MatchesQuery=" + queryDescriptorWithProper + "-" + matchesQuery)
        if (!matchesQuery) {
          feat("ContainsQuery=" + queryDescriptorWithProper + "-" + (den.toLowerCase.contains(queryStr.toLowerCase)));
          feat("StartsWithQuery=" + queryDescriptorWithProper + "-" + (den.toLowerCase.startsWith(queryStr.toLowerCase)));
          feat("EndsWithQuery=" + queryDescriptorWithProper + "-" + (den.toLowerCase.endsWith(queryStr.toLowerCase)));
        }
        val denotationHasParenthetical = den.contains("(") && den.endsWith(")");
        feat("ContainsParenthetical=" + queryDescriptorWithProper + "-" + denotationHasParenthetical);
        if (denotationHasParenthetical) {
          feat("MatchesQueryUpToParen=" + queryDescriptorWithProper + "-" + (den.substring(0, den.indexOf("(")).trim.toLowerCase == queryStr.toLowerCase))
        }
        for(i <- 0 until 4) {
          featUpToVal("CompariableWordsLog-"+i+"=", Math.ceil(Math.log(denotationSim(denIdx)(i) / denotationSimMax(i) * 10000)).asInstanceOf[Int])
          feat("CompariableIsMaxWordSim-"+i+"=" + (denotationSim(denIdx)(i) == denotationSimMax(i)))
          feat("CompariableWordsAboveAvg-"+i+"=" + (denotationSim(denIdx)(i) > denotationSimAvg(i)))
          //featUpToVal("CompariableWordsReweight-"+i+"=", Math.floor(denotationSim(denIdx)(i) / denotationSimMax(i) * 10).asInstanceOf[Int])
        }
        for(i <- 0 until pmingdvals(denIdx).size) {
          featUpToVal("PMINGD-VEC-" + i + "=", Math.ceil(Math.log(pmingdvals(denIdx)(i) / maxpmingd(i) * 10000)).asInstanceOf[Int])
          //featUpToVal("PMINGD-log-VEC-" + i + "=", Math.ceil(Math.log(pmingdvals(denIdx)(i))).asInstanceOf[Int])
          if(maxpmingd(i) == pmingdvals(denIdx)(i)) {
            feat("PMINGD-max-VEC-"+i)
          }
        }
      } else {
        feat("Impossible");
      }
      feats.toArray;
    });
  }
  
  def featurizeQueriesAndDenotations(queries: Seq[Query], denotations: Seq[String], addToIndexer: Boolean): Array[Array[Array[Int]]] = {
    val queryOutcomes = queries.map(query => wikiDB.disambiguateBestGetAllOptions(query));
    val queryNonemptyList = queryOutcomes.map(_.isEmpty);
    val ment = queries.head.originalMent;
    val mentUpToHeadSize = ment.headIdx - ment.startIdx + 1;
    Array.tabulate(queries.size, denotations.size)((queryIdx, denIdx) => {
      val feats = new ArrayBuffer[Int];
      def feat(str: String) = addFeat(str, feats, addToIndexer);
      val query = queries(queryIdx);
      val den = denotations(denIdx);
      if (den == NilToken) {
        feat("NilAndQueryNonempty=" + queryNonemptyList(queryIdx));
      } else if (queryOutcomes(queryIdx).containsKey(den)) {
        val queryDescriptorWithProper = (if (ment.pos(ment.headIdx - ment.startIdx) == "NNP") "PROP" else "NOM") + "-" + query.queryType;
        val queryRank = queryOutcomes(queryIdx).getSortedKeys().indexOf(den);
        feat("Rank=" + queryDescriptorWithProper + "-" + (queryRank + 1))
        val queryStr = query.getFinalQueryStr;
        val matchesQuery = den.toLowerCase == queryStr.toLowerCase;
        feat("MatchesQuery=" + queryDescriptorWithProper + "-" + matchesQuery)
        if (!matchesQuery) {
          feat("ContainsQuery=" + queryDescriptorWithProper + "-" + (den.toLowerCase.contains(queryStr.toLowerCase)));
          feat("StartsWithQuery=" + queryDescriptorWithProper + "-" + (den.toLowerCase.startsWith(queryStr.toLowerCase)));
          feat("EndsWithQuery=" + queryDescriptorWithProper + "-" + (den.toLowerCase.endsWith(queryStr.toLowerCase)));
        }
        val denotationHasParenthetical = den.contains("(") && den.endsWith(")");
        feat("ContainsParenthetical=" + queryDescriptorWithProper + "-" + denotationHasParenthetical);
        if (denotationHasParenthetical) {
          feat("MatchesQueryUpToParen=" + queryDescriptorWithProper + "-" + (den.substring(0, den.indexOf("(")).trim.toLowerCase == queryStr.toLowerCase))
        }
      } else {
        feat("Impossible");
      }
      feats.toArray;
    });
  }
  
  def extractDenotationSetWithNil(queries: Seq[Query], queryDisambigs: Seq[Counter[String]], maxDenotations: Int): Seq[String] = {
    Query.extractDenotationSetWithNil(queries, queryDisambigs, maxDenotations);
//    val choicesEachQuery = queryDisambigs.map(_.getSortedKeys().asScala);
//    val optionsAndPriorities = (0 until queryDisambigs.size).flatMap(i => {
//      val sortedKeys = queryDisambigs(i).getSortedKeys().asScala
//      (0 until sortedKeys.size).map(j => (sortedKeys(j), j * 1000 + i));
//    });
////    choicesEachQuery.foreach(Logger.logss(_));
////    Logger.logss(optionsAndPriorities);
//    val allFinalOptions = Seq(NilToken) ++ optionsAndPriorities.sortBy(_._2).map(_._1).distinct;
//    val finalOptionsTruncated = allFinalOptions.slice(0, Math.min(allFinalOptions.size, maxDenotations));
////    Logger.logss(finalOptions);
//    finalOptionsTruncated;
  }
  
  def getNormalizedLogScores(ex: QueryChoiceExample, weights: Array[Float]) = {
    val scores = featurizeUseCache(ex, false).map(feats => GUtil.scoreIndexedFeats(feats, weights));
    GUtil.logNormalizei(scores);
    scores;
  }
  
  def addUnregularizedStochasticGradient(ex: QueryChoiceExample, weights: Array[Float], gradient: Array[Float]) {
    val allFeats = featurizeUseCache(ex, false);
    val scores = getNormalizedLogScores(ex, weights)
    val correctLogNormalizer = SloppyMath.logAdd(ex.correctQueryIndices.map(scores(_)));
    for (i <- 0 until ex.queries.size) {
      if (ex.correctQueryIndices.contains(i)) {
        GUtil.addToGradient(allFeats(i), Math.exp(scores(i) - correctLogNormalizer).toFloat, gradient);
      }
      GUtil.addToGradient(allFeats(i), -Math.exp(scores(i)).toFloat, gradient);
    }
  }
  
  def computeLogLikelihood(ex: QueryChoiceExample, weights: Array[Float]): Float = {
    val scores = getNormalizedLogScores(ex, weights)
    val correctScores = ex.correctQueryIndices.map(scores(_));
    SloppyMath.logAdd(correctScores).toFloat;
  }
  
  def predictIdx(ex: QueryChoiceExample, weights: Array[Float]) = {
    GUtil.argMaxIdxFloat(getNormalizedLogScores(ex, weights));
  }
  
  def predictEquivalentIdxSet(ex: QueryChoiceExample, weights: Array[Float]): Set[Int] = {
    val scores = getNormalizedLogScores(ex, weights);
    val denotationScores = new Counter[String];
    for (i <- 0 until ex.denotations.size) {
      denotationScores.incrementCount(ex.denotations(i), Math.exp(scores(i)));
    }
//    val denotationMap = new HashMap[String,HashSet[Int]];
//    for (i <- 0 until ex.denotations.size) {
//      if (!denotationMap.contains(ex.denotations(i))) {
//        denotationMap.put(ex.denotations(i), new HashSet[Int]);
//      }
//      denotationMap(ex.denotations(i)) += i;
//    }
//    val denotationScores = new Counter[String];
//    for (denotation <- denotationMap.keySet) {
//      denotationScores.incrementCount(denotation, Math.exp(SloppyMath.logAdd(denotationMap(denotation).map(i => scores(i)).toArray)));
//    };
    ex.denotations.zipWithIndex.filter(denotationAndIdx => denotationAndIdx._1 == denotationScores.argMax()).map(_._2).toSet;
  }
}

@SerialVersionUID(1L)
class QueryChooser(val featureIndexer: Indexer[String],
                   val weights: Array[Float]) extends Serializable {

  def pickQuery(queries: Seq[Query], wikiDB: WikipediaInterface): Query = {
    val qcComputer = new QueryChoiceComputer(wikiDB, featureIndexer);
    val denotations = queries.map(query => wikiDB.disambiguateBestNoDisambig(query));
    val ex = new QueryChoiceExample(queries, denotations, Array[Int]());
//    queries(qcComputer.predictIdx(ex, weights));
    val eqIdxSet = qcComputer.predictEquivalentIdxSet(ex, weights)
    queries(eqIdxSet.head);
  }
}

object QueryChooser {
  
  def getBasicQueryChooser = {
    val fi = new Indexer[String];
    fi.getIndex("FirstNonempty=true");
    fi.getIndex("FirstNonempty=false");
    new QueryChooser(fi, Array(1F, -1F))
  }
  
  def extractExamples(corefDocs: Seq[CorefDoc], goldWikification: CorpusWikiAnnots, wikiDB: WikipediaInterface) = {
    val exs = new ArrayBuffer[QueryChoiceExample];
    var numImpossible = 0;
    for (corefDoc <- corefDocs) {
      val docName = corefDoc.rawDoc.docID
      for (i <- 0 until corefDoc.predMentions.size) {
        val ment = corefDoc.predMentions(i);
        val goldLabel = getGoldWikification(goldWikification(docName), ment)
        if (goldLabel.size >= 1) {
          val queries = Query.extractQueriesBest(ment, true);
          val denotations = queries.map(wikiDB.disambiguateBestNoDisambig(_));
          val correctIndices = denotations.zipWithIndex.filter(denotationIdx => isCorrect(goldLabel, denotationIdx._1)).map(_._2);
          if (!correctIndices.isEmpty) {
            exs += new QueryChoiceExample(queries, denotations, correctIndices.toArray);
          } else {
            numImpossible += 1;
          }
        }
      }
    }
    Logger.logss(exs.size + " possible, " + numImpossible + " impossible");
    exs;
  }
  
  // Command line options
  val trainDataPath = "data/ace05/train";
  val testDataPath = "data/ace05/dev";
  val wikiPath = "data/ace05/ace05-all-conll-wiki"
  val wikiDBPath = "models/wiki-db-ace.ser.gz"
  
  def main(args: Array[String]) {
    LightRunner.initializeOutput(QueryChooser.getClass());
    LightRunner.populateScala(QueryChooser.getClass(), args)
//    val wikiAnnotsPath = "data/ace05/ace-annots-multi.ser"
//    val goldWikification = GUtil.load(wikiAnnotsPath).asInstanceOf[CorpusWikiAnnots];
    val goldWikification = WikiAnnotReaderWriter.readStandoffAnnotsAsCorpusAnnots(wikiPath)
    val wikiDB = GUtil.load(wikiDBPath).asInstanceOf[WikipediaInterface];
    
    val assembler = CorefDocAssembler(Language.ENGLISH, true);
    val trainDocs = ConllDocReader.loadRawConllDocsWithSuffix(trainDataPath, -1, "", Language.ENGLISH);
    val trainCorefDocs = trainDocs.map(doc => assembler.createCorefDoc(doc, new MentionPropertyComputer(None)));
    
    val testDocs = ConllDocReader.loadRawConllDocsWithSuffix(testDataPath, -1, "", Language.ENGLISH);
    val testCorefDocs = testDocs.map(doc => assembler.createCorefDoc(doc, new MentionPropertyComputer(None)));
    
    val trainExs = extractExamples(trainCorefDocs, goldWikification, wikiDB);
    val testExs = extractExamples(testCorefDocs, goldWikification, wikiDB);
    
    val featIndexer = new Indexer[String]
    val qcComputer = new QueryChoiceComputer(wikiDB, featIndexer);
    for (trainEx <- trainExs) {
      qcComputer.featurizeUseCache(trainEx, true);
    }
    Logger.logss(featIndexer.size + " features");
    val gt = new GeneralTrainer[QueryChoiceExample]();
    val weights = gt.trainAdagrad(trainExs, qcComputer, featIndexer.size, 1.0F, lambda = 1e-8F, batchSize = 100, numItrs = 10);
    
    if (weights.size < 1000) {
      val weightsCounter = new Counter[String];
      for (i <- 0 until weights.size) {
        weightsCounter.incrementCount(featIndexer.getObject(i), weights(i));
      }
      Logger.logss(weightsCounter)
    }
    
    val baselineIndexer = new Indexer[String];
    baselineIndexer.getIndex("FirstNonempty=true");
    baselineIndexer.getIndex("FirstNonempty=false");
    evaluate(testExs, new QueryChoiceComputer(wikiDB, baselineIndexer), Array(1F, -1F), wikiDB);
    evaluate(testExs, qcComputer, weights, wikiDB, true);
    GUtil.saveGz(new QueryChooser(featIndexer, weights), "models/querychooser.ser.gz")
    LightRunner.finalizeOutput();
  }
  
  def evaluate(testExs: Seq[QueryChoiceExample], qcComputer: QueryChoiceComputer, weights: Array[Float], wikiDB: WikipediaInterface, print: Boolean = false) {
    var numCorrect = 0;
    var numCorrectNil = 0;
    var numNil = 0;
    for (i <- 0 until testExs.size) {
      val isNil = testExs(i).correctQueryIndices.map(idx => wikiDB.disambiguateBestNoDisambig(testExs(i).queries(idx)) == NilToken).reduce(_ || _);
      if (isNil) {
        numNil += 1;
      }
//      val choice = qcComputer.predictIdx(testExs(i), weights)
      val choice = qcComputer.predictEquivalentIdxSet(testExs(i), weights).head
      if (testExs(i).correctQueryIndices.contains(choice)) {
        numCorrect += 1;
        if (isNil) numCorrectNil += 1;
      } else {
        if (print) {
          val scores = qcComputer.getNormalizedLogScores(testExs(i), weights);
          (0 until testExs(i).queries.size).foreach(j => {
            Logger.logss(testExs(i).queries(j).getFinalQueryStr + "\t" + testExs(i).denotations(j) + "\t" +
                         (if (testExs(i).correctQueryIndices.contains(j)) "gold" else "") + "\t" + scores(j) + "\t" + (if (choice == j) "chosen" else ""));
          });
        }
      }
      testExs(i).cachedFeatsEachQuery = null;
    }
    Logger.logss("Total: " + GUtil.renderNumerDenom(numCorrect, testExs.size))
    Logger.logss("Nil: " + GUtil.renderNumerDenom(numCorrectNil, numNil))
    Logger.logss("Non-nil: " + GUtil.renderNumerDenom(numCorrect - numCorrectNil, testExs.size - numNil))
  }
}
