
#L-----------------------------------------------------------------------Library
import PyPDF2
#----------------------------------------------Function to Convert a PDF to TXT
def pdfToTxt(file):
    fileReader = PyPDF2.PdfFileReader(open(file,'rb'))
    countpage = fileReader.getNumPages()
    count = 0
    Txt = []
    while count < countpage:    
        pageObj = fileReader.getPage(count)
        count +=1
        t = pageObj.extractText()
        Txt.append(t)
        Txt = str(Txt)
        Txt = Txt.replace("\\n", "")
        Txt = Txt.lower()
    return (Txt)
