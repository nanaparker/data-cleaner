var dropArea = document.getElementById("grid");
var second_dropArea = document.getElementById("cross_grid");

// If user drags file over drop area
dropArea.addEventListener('dragover', (event)=>{
    event.preventDefault(); // preventing default behaviour
    dropArea.className = 'grid dragover';
    dropArea.classList.add('active');
});


// If user leaves the drop area
dropArea.addEventListener('dragleave', ()=>{
    dropArea.classList.remove('active');
    dropArea.className = 'grid';
});


// If user drops file on the drop 
dropArea.addEventListener('drop', (event)=>{
    event.preventDefault(); // preventing default behaviour
    dropArea.className = 'grid';
    
    // getting user selected file
    file = event.dataTransfer.files

    if (file.length > 1){
        alert('Only one(1) file required');
    } 
    else {
        let new_file = file[0];
        let filetype = new_file.type;
        let validExts = 'text/csv';
        
        if (validExts.includes(filetype)){
            HandleDrop(new_file);
        } else {
            alert('This is not a CSV file!');
            dropArea.classList.remove('active');
        }
    }

});



// If user drags file over drop area
second_dropArea.addEventListener('dragover', (event)=>{
    event.preventDefault(); // preventing default behaviour
    second_dropArea.className = 'cross_grid dragover';
    second_dropArea.classList.add('active');
});


// If user leaves the drop area
second_dropArea.addEventListener('dragleave', ()=>{
    second_dropArea.classList.remove('active');
    second_dropArea.className = 'cross_grid';
});


// If user drops file on the drop 
second_dropArea.addEventListener('drop', (event)=>{
    event.preventDefault(); // preventing default behaviour
    second_dropArea.className = 'cross_grid';
    
    // getting user selected file
    file = event.dataTransfer.files

    if (file.length > 2){
        alert('Only two(2) files required');
    } 
    else {
        console.log(file[0])
        console.log(file[1])
        var new_file2 = file[0][0];
        var filetype2 = file[0].type;
        
        var new_file3 = file[1][0];
        var filetype3 = file[1].type;
        var validExts = 'text/csv';
        
        if (validExts.includes(filetype2) && validExts.includes(filetype3)){
            var finale = [file[0], file[1]];
            handleDrop(finale);

            console.log(datasets2)
        } else {
            alert('This is not a CSV file!');
            second_dropArea.classList.remove('active');
        }
    }

});


var datasets = {};
var datasets2 = {};


// Function for Two File Check
var handleDrop = function(files) {
  
  var reader = new FileReader();

  reader.onload = (function(file) {
      return function(event) {
        datasets2[file.name.toLowerCase()] = event.target.result;

        var div = document.createElement("div");
        var src = "https://cdn0.iconfinder.com/data/icons/office/512/e42-512.png";
        div.className = "cross_dataset";
        div.innerHTML = [
            "<img class='csv-image' src='", src, "' title='", encodeURI(file.name), 
            "' width=30px height=30px/>", 
            "<p class='csv-file'>", file.name, "</p>"
        ].join('');
        
        document.getElementById("cross_sidebar").appendChild(div);
      };
  })(files[0]);
  reader.readAsText(files[0]);
  
  var reader_2 = new FileReader();

  reader_2.onload = (function(file) {
      return function(event) {
        datasets2[file.name.toLowerCase()] = event.target.result;

        var div = document.createElement("div");
        var src = "https://cdn0.iconfinder.com/data/icons/office/512/e42-512.png";
        div.className = "cross_dataset2";
        div.innerHTML = [
            "<img class='csv-image' src='", src, "' title='", encodeURI(file.name), 
            "' width=30px height=30px/>", 
            "<p class='csv-file'>", file.name, "</p>"
        ].join('');
        
        document.getElementById("cross_sidebar").appendChild(div);
      };
  })(files[1]);
  reader_2.readAsText(files[1]);
};


// Function for Single File Check
var HandleDrop = function(files) {
  var reader = new FileReader();

  reader.onload = (function(file) {
      return function(event) {
        datasets[file.name.toLowerCase()] = event.target.result;

        var div = document.createElement("div");
        var src = "https://cdn0.iconfinder.com/data/icons/office/512/e42-512.png";
        div.className = "datasets";
        div.innerHTML = [
            "<img class='csv-image' src='", src, "' title='", encodeURI(file.name), 
            "' width=30px height=30px/>", 
            "<p class='csv-file'>", file.name, "</p>"
        ].join('');
        
        document.getElementById("sidebar").appendChild(div);
      };
  })(files);
  reader.readAsText(files);
};