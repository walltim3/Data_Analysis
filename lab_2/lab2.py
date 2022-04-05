#!/usr/bin/env python3

import re
import sys
import datetime

def parse_rss(t, source):
    
    #Розбиття файлу по рядкам і склеювання рядків через пропуск
    rss =t.split('\n')
    t=""
    for i in range(len(rss)):
        t=t+" "+rss[i]

        #Видалення перших пропусків
        t=re.sub('^\s','',t)
    
    #Формування заголовків
    try:
        title = re.findall('<title>(.+?)<\/title>', t)[0]
    except:
        if  re.findall('<title>(.+?)<\/title>', t) == []:
            title = ""

    #Формування описів
    try:
        text = re.findall('<title>(.*?)<\/title>', t)[0]
    except:
        if re.findall('<title>(.*?)<\/title>', t) == []:
            text = ""

    #Формування описів
    try:
        desc = re.findall('<description>(.*?)<\/description>', t)[0]
    except:
        if re.findall('<description>(.*?)<\/description>', t) == []:
            desc = ""

    #Формування гіперпосилань
    try:
        link = re.findall('<link>(.+?)<\/link>', t)[0]
    except:
        if re.findall('<link>(.+?)<\/link>', t) == []:
            link = ""
    #Формування дати і часу
    
    try:
        tim = re.findall('<pubDate>(.+?)<\/pubDate>', t)[0]
    except:
        if e.findall('<pubDate>(.+?)<\/pubDate>', t) == []:
           tim = "" 

    # зміна формату на "YY-MM-MMTHH:MM:00Z"
    reformat_1 = lambda data : datetime.datetime.strptime(data, "%d %b %Y %H:%M:%S %z").strftime("%Y-%m-%dT%H:%M:%SZ")
    reformat_2 = lambda data : datetime.datetime.strptime(data, "%a, %d %b %Y %H:%M:%S %z").strftime("%Y-%m-%dT%H:%M:%SZ")
    
    try:
        tim = reformat_1(tim)
    except ValueError as e:
        try:
            tim = reformat_2(tim)
        except ValueError as e:
            sys.exit(e)

    #Виведення результатів
    print ("{\n\"title\":\""+title+"\",")

    #Специфічна обробка тексту
    text=re.sub('[\s\-]*$','',text)
    text=re.sub('"','\"',text)
    text=re.sub('\'','&amp;',text)

    #Подальша виведення результатів
    print ("\"textBody\":\""+desc+"\",")
    print ("\"source\":\""+source+"\",")
    print ("\"PubDate\":\""+tim+"\",")
    print ("\"URL\":\"",link,"\"\n}")
    print(",")
    
def parse_atom(t, souce):
    #Розбиття файлу по рядкам і склеювання рядків через пропуск
    atom =t.split('\n')
    t=""
    for i in range(len(atom)):
        t=t+" "+atom[i]

        #Видалення перших пропусків
        t=re.sub('^\s','',t)    

    #Формування заголовків
    try:
        title = re.findall('<title>(.+?)<\/title>', t)[0]
    except:
        if  re.findall('<title>(.+?)<\/title>', t) == []:
            title= ""

    #Розмірність заголовків
    x=len(title)

    #Формування описів
    try:
        text = re.findall('<title>(.*?)<\/title>', t)[0]
    except:
        if re.findall('<title>(.*?)<\/title>', t) == []:
            text = ""

    #Формування описів
    try:
        desc = re.findall('<summary type="html"><\!\[CDATA\[(.*?)\]\]><\/summary>', t)[0]
    except:
        if re.findall('<summary type="html"><\!\[CDATA\[(.*?)\]\]><\/summary>', t) == []:
            desc = ""
    
    #Формування гіперпосилань
    try:
        link = re.findall('<id>(.+?)<\/id>', t)[0]
    except:
        if re.findall('<id>(.+?)<\/id>', t) == []:
            link = ""

    #Формування дати і часу в форматі  "YY-MM-MMTHH:MM:00Z"
    try:
        tim = re.findall('<updated>(.+?)<\/updated>', t)[0]
    except:
        if re.findall('<updated>(.+?)<\/updated>', t) == []:
            tim  = ""

    #Виведення результатів
        
    #Специфічна обробка тексту
    text=re.sub('[\s\-]*$','',text)
    text=re.sub('"','\"',text)
    text=re.sub('\'','&amp;',text)

    #Формування файлу
    print ("{\n\"title\":\""+title+"\",")
    print ("\"textBody\":\""+desc+"\",")
    print ("\"source\":\""+source+"\",")
    print ("\"PubDate\":\""+tim+"\",")
    print ("\"URL\":\"",link,"\"\n}")
    print(",")

try:
    filename = sys.argv[1]
except:
    sys.exit("Usage: ./script <rss_feed_filename>")

with open(filename, "r", encoding = 'utf-8') as f:

    # читання файлу построчно для більшої продуктивності

    status = ""
    chunk = ["", "", ""]
    source = ""
    inside = False

    for line in f:

        atom = """<feed xmlns=\".+/Atom\">"""
        rss = """<rss version="2.0">"""
    
          
        format = re.findall(rss, line)
        if format == []:
            format = re.findall(atom, line)
            if format != []:
                status = "atom"
                chunk = ["<entry>", "<\/entry>", ""]

        else:
            status = "rss"
            chunk = ["<item>", "<\/item>", ""]  
    
    
        #Перший заголовок – назва фіду, далі – його специфічна обробка
        if source == "":        
            candidates=re.findall('<title>(.+?)<\/title>', line)
            if candidates != []:
                source = candidates[0]
                source=re.sub('[\s\-]*$','',source)
                source=re.sub('"','\"',source)

        # split into chunks for faster parsing
        
        if status != "":
            if inside:
                chunk[2] += line

            if re.findall(chunk[0], line) != []:
                inside = True

            if re.findall(chunk[1], line) != []:
                inside = False
                if status == "atom":
                    parse_atom(chunk[2], source)
                if status == "rss":
                    parse_rss(chunk[2], source)
                chunk[2] = ""

