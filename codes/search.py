# Packages
import os
from PyPDF2 import PdfReader
import time
import shutil

'''
within the project two folders are needed: input and transform.
output is the folder that contains the articles that will be searched
transform is the folder to which the articles that contains the matches will be send

'''

files = os.listdir('./output/') #Get list of articles from folder output


#the list "modes" is the list that contains the keywords to search in the articles
modes = ['transform']

# Automatic folder creation related to keywords in list "modes"

for i in range(0, len(modes)):
    nome = modes[i].strip()
    nome = nome.replace(" ", "")
    nome = nome[:-9]
    dir_path = './transform/' + nome
    if not os.path.exists(dir_path):
        os.makedirs(dir_path)

############################################################3

#Performs search of each keyword in "modes" and moves the articles with matches
#to a folder named after the keyword

for j in range(0, len(modes)):
    nPaper = 0
    start = time.time()
    tgt = modes[j]
    dire = tgt[:-10]
    dire = dire.replace(" ", "")
    dire = dire + '/'
    path = './transform/' + dire
    if not os.path.exists(path):
        os.makedirs(path)
    f = open(os.path.join(path, 'log.dat'), 'w')
    f.write('Exit Log\n')

    for k in range(0, len(files)):
        try:
            reader = PdfReader('./output/' + files[k], strict=False)
            nPages = len(reader.pages)

            nString = 0
            for i in range(0, nPages - 1):
                page = reader.pages[i]
                text = page.extract_text()
                text = text.lower()
                nString = nString + text.count(tgt)

            if nString > 0:
                nPaper = nPaper + 1
                original = './output/' + files[k]
                path = './transform/' + dire + files[k]
                shutil.copyfile(original, path)
                stri = "Article " + str(files[k]) + ' has ' + str(nString) + ' matches'
                f.write(stri + '\n')

            print(nPaper, k + 1, nPaper / (k + 1) * 100, '%', )

        except Exception as e:

            print(f"An error occurred while processing file {files[k]}: {str(e)}")

            continue

    end = time.time()
    tempoDecor = str((end - start) / 60.0)
    tempoDecor = tempoDecor[0:4]
    stri = 'Took ' + tempoDecor + ' minutes to perform the search'
    f.write(stri + '\n')
    f.close()
