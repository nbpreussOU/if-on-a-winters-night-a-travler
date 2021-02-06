# WSB Title Scrape
import pandas
import requests
# from time import gmtime, strftime
import praw
# from praw.models import MoreComments
from psaw import PushshiftAPI
import datetime as dt

def scrapeTitles():
    reddit = praw.Reddit(client_id="7HuVGrxZwvj0fA",
                     client_secret="07pRQvVQW9suS_oPCDeYGRp4yeQ",
                     user_agent="jupyter_notebook:test_reddit_api /u/Asterisk13")
    # set up connection

    # dates
    api = PushshiftAPI(reddit)
    start = int(dt.datetime(2021,1,30).timestamp())
    end = int(dt.datetime(2021,1,27).timestamp())

    # empty lists to put data

    titleList = []

    for submission in api.search_submissions(before=start, after=end, subreddit="wallstreetbets"):
        titleList.append(getTitle(submission))
    
    #convert to csv
    df1 =pandas.DataFrame(titleList) 
    df1.to_csv("titleListWeek2Part2.csv")



def getTitle(submission):
    record = { #title of each column in our csv file
        "sub_id": submission.id,
        "sub_fullname": submission.name,
        "title": submission.title
    }
    return record

scrapeTitles()
