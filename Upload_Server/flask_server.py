import os
from flask import Flask, request, render_template, flash, redirect
from werkzeug.utils import secure_filename

app = Flask(__name__)

UPLOAD_FOLDER = 'upload'
os.makedirs(UPLOAD_FOLDER, exist_ok=True)
app.config['UPLOAD_FOLDER'] = UPLOAD_FOLDER

@app.route('/', methods=['GET', 'POST'])
def index():

    if request.method == 'GET':
        #present the page index.html
        return render_template('index.html')
        

    
    if request.method == 'POST':

        if 'file' not in request.files:
            flash('No file part')
            return redirect(request.url)
        file = request.files['file']

        if file.filename == '':
            flash('No selected file')
            return redirect(request.url)

        if file :
            filename = secure_filename(file.filename)
            
            filename_alt = filename
            i = 0

            while os.path.exists(os.path.join( app.config['UPLOAD_FOLDER'] , filename)):
                i = i + 1
                filename = filename_alt + '_' + str(i)
                
            file.save(os.path.join(app.config['UPLOAD_FOLDER'],filename))
            return """OK"""


if __name__ == '__main__':
    app.run(host = '0.0.0.0',port=80,threaded=True, debug=False)
