# WSB Title Scrape

# For info on using PRAW to access the Reddit API, please read this site: https://praw.readthedocs.io/en/latest/
# To use PRAW, you must first register an app with Reddit to access their API, info on that here: https://www.reddit.com/wiki/api

import pandas
import requests
# from time import gmtime, strftime
import praw
# from praw.models import MoreComments
from psaw import PushshiftAPI
import datetime as dt

# Insert the Client ID, Secret, and User_Agent for whatever Reddit account is associated with your registered app

def scrapeTitles():
    reddit = praw.Reddit(client_id="insert_client_id_here",
                     client_secret="insert_client_secret_here",
                     user_agent="insert_user_agent_here")
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
