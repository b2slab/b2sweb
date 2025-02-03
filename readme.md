# b2slab website

This is the b2slab website. 


# Contributing

Guys and girls, an important note:
- Please I need each one of you to (staff/phd/visitors and all)  [fill out this form](https://docs.google.com/forms/d/e/1FAIpQLSd6AMQYIAMdZSBA5nBdCXZDMksNtTq_mSzL29vMAZ1nTflOWA/viewform?usp=dialog).  You will need a picture of yourself, square format, of around 640x640px approx tp fill the form. 
- This is required as the new website will pull everyone of us from the linked google spreadsheet. You can see the tests if this on the deployed new site at [http://b2slab.upc.edu](http://b2slab.upc.edu).
Some random notes about the website:
- Now the website pulls the team info from the linked  google spreadsheet
- It also pulls the publications list from [futur.uypce.d](https://futur.upc.edu/B2SLab))](https://futur.upc.edu/B2SLab). Remember you need to add all your production to [drac](https://drac.upc.edu/) so it is uploaded there.
- We will all  need to create personal websites. 
- The website is coded with [quarto](https://quarto.org). This means that is essentially build with markdown. You can create a quarto document within Rstudio with your personal website and add it to the ~web-and-comms  channel.  Please join all that channel as we will coordinate the content on there and release this one. I will upload at some point a nice template for a personal site.  

Please find at teams/alexandre-perera-lluna folder  a template for personal website, if you keep the r cells you will get your production automcatically placed form futur. To get the link in futur you need to:

- search for your profile at [https://futur.upc.edu](https://futur.upc.edu)
- Filter for *Article en revista indexada*
- Click "Exportar"
- Click Gestior de referencies
- Click OK
- A file will download, discard the downloaded file, got to downloads in your browser and copy the address of the file, something like "https://futur.upc.edu/RIS/AlexandrePereraLluna/as/YXV0b3JpYUFydGljbGVSZXZpc3RhSW5kZXhhZGE=/?locale=en", add this file in the R section on the qmd of the template. 
- Create your personal page and add a forder with your name. Create a commit to the [github](https://github.com/b2slab/b2sweb) in  the`team/`folder, or send the zip to @alex   


# Deployment

call:

```
bash deploy.sh
```
You will need credentials to the b2sserver.  

