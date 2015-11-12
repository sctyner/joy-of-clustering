from wand.image import Image

#save file names in a list
paintings = ["" for x in range(1,243)]
for x in range(0, 242):
    paintings[x] = "bobross%d.jpg" % (x+1)
#rescale all all images to 20 X 20 pixels
for x in range(0,242):
	with Image(filename=paintings[x]) as img:
		with img.clone() as ic:
			ic.resize(20, 20)
			newname = "bobrossnew%d.jpg" % (x+1)
			ic.save(filename=newname)
#copy-paste results of this into command line to convert jpgs to txt files
for x in range(0,242):
	name = "convert bobrossnew%d.jpg bobross%d.txt" % (x+1,x+1)	    
	print(name)
	