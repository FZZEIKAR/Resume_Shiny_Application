#----------------------------------------------------------------------Libraries
import re
import numpy as np
#---------------------------------------Function to calculate keyword importance
def keyword_importance(keyword,file):
    Txt=pdfToTxt(file)
    keyword_list = re.findall(keyword,Txt)
    number_of_times_word_appeared =len(keyword_list)
    tf = number_of_times_word_appeared/float(len(Txt))
    #idf = np.log((1)/float(number_of_times_word_appeared))
    #tf_idf = tf*idf
    return number_of_times_word_appeared,tf
