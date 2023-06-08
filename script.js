window.addEventListener('DOMContentLoaded', (event) => {
    const projectsSection = document.getElementById('projects');
    const projects = projectsSection.getElementsByTagName('a');

    for (let i = 0; i < projects.length; i++) {
        projects[i].addEventListener('click', function(event) {
            const confirmMessage = 'Are you sure you want to view this project?';
            if (!confirm(confirmMessage)) {
                event.preventDefault();
            }
        });
    }
});
