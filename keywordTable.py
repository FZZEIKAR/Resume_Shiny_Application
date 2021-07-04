#----------------------------------------------------------------------Libraries
import re
import pandas as pd
#---------------------------------Function to count and construct keywords table
def keywordTable(mot_clé,file):
  Txt=pdfToTxt(file)
  mot_clé = re.findall(mot_clé,Txt)
  df = pd.DataFrame(list(set(mot_clé)),columns=['mot_clé'])
  df['occurrence'] = df['mot_clé'].apply(lambda x: keyword_importance(x,file)[0])
  df['tf'] = df['mot_clé'].apply(lambda x: keyword_importance(x,file)[1])
  #df['idf'] = df['mot_clé'].apply(lambda x: keyword_importance(x,file)[2])
  #df['tf_idf'] = df['mot_clé'].apply(lambda x: keyword_importance(x,file)[3])
  return(df)

