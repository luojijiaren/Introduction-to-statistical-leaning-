
# coding: utf-8

# In[14]:

#!/usr/bin/env python

"""

Minimal Example

===============

Generating a square wordcloud from the US constitution using default arguments.

"""



from os import path

from wordcloud import WordCloud

import pandas as pd

df = pd.read_csv(r'C:\Users\Huayi\Desktop\Slide\DS502\project\clean_loan.csv')

purposelist = df['purpose'].tolist()
purposestr = ' '.join(statelist)

# Generate a word cloud image

wordcloud = WordCloud(background_color = 'white').generate(purposestr)



# Display the generated image:

# the matplotlib way:

import matplotlib.pyplot as plt

plt.imshow(wordcloud)

plt.axis("off")



# lower max_font_size

wordcloud = WordCloud(background_color = 'white',max_font_size=40).generate(purposestr)

plt.figure()

plt.imshow(wordcloud)

plt.axis("off")

plt.show()



# The pil way (if you don't have matplotlib)

#image = wordcloud.to_image()

#image.show()


# In[4]:




# In[ ]:



