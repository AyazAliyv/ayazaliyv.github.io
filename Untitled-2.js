fetch('https://api.github.com/users/<AyazAliyv>/repos')
    .then(response => response.json())
    .then(data => {
        // Do something with your data
        console.log(data);
    });
