library(here)
library(base64enc)

# Ensure paths are loaded
if (!exists("paths")) {
  source(file.path(here::here(), "code", "0_setup.R"))
}

# Ensure cu_run is loaded
if (!exists("cu_run")) {
  load(file.path(paths$CU, "cu_run.Rds"))
}

cat("Packaging compiled CU reports into self-contained HTML dashboard...\n")

# Loop over CUs and generate JSON items
cu_items <- list()
for (i in 1:nrow(cu_run)) {
  cu_code <- cu_run$FULL_CU_IN[i]
  cu_name <- cu_run$CU_NAME[i]
  cu_species <- cu_run$SPECIES_NAME[i]
  
  # Path to compiled individual report
  report_file <- file.path(paths$reports, "CU_reports", paste0(cu_code, "_CVIS_Data_report.html"))
  
  if (file.exists(report_file)) {
    # Base64-encode the HTML file
    base64_data <- base64enc::base64encode(report_file)
    has_report <- "true"
  } else {
    base64_data <- ""
    has_report <- "false"
  }
  
  # Escape quotes for safety in JS
  cu_name_esc <- gsub("'", "\\\\'", cu_name, fixed = TRUE)
  cu_name_esc <- gsub('"', '\\\\"', cu_name_esc, fixed = TRUE)
  
  item_str <- sprintf(
    "{ code: '%s', name: '%s', species: '%s', hasReport: %s, base64: '%s' }",
    cu_code, cu_name_esc, cu_species, has_report, base64_data
  )
  cu_items[[i]] <- item_str
}

cu_json_array <- paste0("const cuData = [\n  ", paste(cu_items, collapse = ",\n  "), "\n];")

# HTML Template for Dashboard
html_template <- r"---(<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>CVIS Supplemental CU Data Report</title>
  <link href="https://fonts.googleapis.com/css2?family=Outfit:wght@300;400;500;600;700;800&family=Inter:wght@300;400;500;600;700&display=swap" rel="stylesheet">
  <style>
    :root {
      --primary: #1A365D;
      --primary-light: #2B6CB0;
      --primary-hover: #2B6CB0;
      --bg: #F7FAFC;
      --sidebar-bg: #1A202C;
      --sidebar-hover: #2D3748;
      --text: #2D3748;
      --text-muted: #718096;
      --border: #E2E8F0;
      --active-bg: #2B6CB0;
      --active-text: #FFFFFF;
    }
    
    * {
      box-sizing: border-box;
      margin: 0;
      padding: 0;
    }
    
    body {
      font-family: 'Inter', sans-serif;
      background-color: var(--bg);
      color: var(--text);
      display: flex;
      height: 100vh;
      overflow: hidden;
    }
    
    /* Sidebar styling */
    .sidebar {
      width: 320px;
      background-color: var(--sidebar-bg);
      color: #EDF2F7;
      display: flex;
      flex-direction: column;
      border-right: 1px solid var(--border);
      flex-shrink: 0;
    }
    
    .sidebar-header {
      padding: 20px;
      border-bottom: 1px solid #2D3748;
    }
    
    .sidebar-header h1 {
      font-family: 'Outfit', sans-serif;
      font-size: 1.25rem;
      font-weight: 700;
      color: #FFFFFF;
      margin-bottom: 5px;
    }
    
    .sidebar-header p {
      font-size: 0.75rem;
      color: #A0AEC0;
    }
    
    .btn-home {
      display: block;
      width: 100%;
      text-align: left;
      background: none;
      border: none;
      color: #A0AEC0;
      font-size: 0.85rem;
      font-weight: 600;
      margin-top: 10px;
      cursor: pointer;
      transition: color 0.2s;
    }
    
    .btn-home:hover {
      color: #FFFFFF;
    }
    
    .search-container {
      padding: 15px 20px;
      border-bottom: 1px solid #2D3748;
    }
    
    .search-input {
      width: 100%;
      padding: 10px 15px;
      background-color: #2D3748;
      border: 1px solid #4A5568;
      border-radius: 6px;
      color: #FFFFFF;
      font-size: 0.85rem;
      outline: none;
      transition: border-color 0.2s;
    }
    
    .search-input:focus {
      border-color: var(--primary-light);
    }
    
    .cu-list {
      flex: 1;
      overflow-y: auto;
      padding: 15px 20px;
    }
    
    .species-group {
      margin-bottom: 20px;
    }
    
    .species-title {
      font-family: 'Outfit', sans-serif;
      font-size: 0.8rem;
      font-weight: 700;
      text-transform: uppercase;
      letter-spacing: 0.05em;
      color: #A0AEC0;
      margin-bottom: 8px;
      padding-left: 5px;
    }
    
    .cu-item {
      display: block;
      padding: 8px 12px;
      border-radius: 6px;
      color: #CBD5E0;
      text-decoration: none;
      font-size: 0.85rem;
      margin-bottom: 4px;
      cursor: pointer;
      transition: all 0.2s;
    }
    
    .cu-item:hover {
      background-color: var(--sidebar-hover);
      color: #FFFFFF;
    }
    
    .cu-item.active {
      background-color: var(--active-bg);
      color: var(--active-text);
      font-weight: 600;
    }
    
    .cu-item.disabled {
      opacity: 0.4;
      cursor: not-allowed;
      pointer-events: none;
    }
    
    /* Main Content Area */
    .content-area {
      flex: 1;
      display: flex;
      flex-direction: column;
      height: 100%;
      position: relative;
    }
    
    iframe {
      width: 100%;
      height: 100%;
      border: none;
      background-color: #FFFFFF;
      display: none;
    }
    
    /* About Landing Screen */
    .about-screen {
      padding: 50px;
      overflow-y: auto;
      max-width: 900px;
      margin: 0 auto;
      height: 100%;
      display: flex;
      flex-direction: column;
      gap: 30px;
    }
    
    .about-header h2 {
      font-family: 'Outfit', sans-serif;
      font-size: 2.25rem;
      font-weight: 800;
      color: var(--primary);
      margin-bottom: 10px;
    }
    
    .about-header p {
      font-size: 1.1rem;
      color: var(--text-muted);
      line-height: 1.6;
    }
    
    .about-section {
      background: #FFFFFF;
      padding: 25px;
      border-radius: 8px;
      border: 1px solid var(--border);
      box-shadow: 0 1px 3px rgba(0,0,0,0.05);
    }
    
    .about-section h3 {
      font-family: 'Outfit', sans-serif;
      font-size: 1.25rem;
      font-weight: 700;
      color: var(--primary);
      margin-bottom: 15px;
    }
    
    .about-section p, .about-section li {
      font-size: 0.95rem;
      line-height: 1.6;
      margin-bottom: 10px;
    }
    
    .about-section ul {
      padding-left: 20px;
      margin-bottom: 15px;
    }
    
    .disclaimer-box {
      background-color: #FFFDF5;
      border: 1px solid #F6AD55;
      padding: 20px;
      border-radius: 8px;
      color: #C05621;
    }
    
    .disclaimer-box h4 {
      font-family: 'Outfit', sans-serif;
      font-size: 1.05rem;
      font-weight: 700;
      margin-bottom: 8px;
      display: flex;
      align-items: center;
      gap: 6px;
    }
    
    .disclaimer-box p {
      font-size: 0.9rem;
      line-height: 1.6;
    }
  </style>
</head>
<body>

  <!-- Sidebar -->
  <div class="sidebar">
    <div class="sidebar-header">
      <h1>CVIS Database</h1>
      <p>Climate Vulnerability Suite</p>
      <button class="btn-home" onclick="showAbout()">&#127969; Home / About Project</button>
    </div>
    <div class="search-container">
      <input type="text" id="search-box" class="search-input" placeholder="Search Conservation Units..." onkeyup="filterCUs()">
    </div>
    <div class="cu-list" id="cu-list-container">
      <!-- Dynamic list injected by JS -->
    </div>
  </div>

  <!-- Main Viewer Content -->
  <div class="content-area">
    <!-- About Landing screen -->
    <div class="about-screen" id="about-landing">
      <div class="about-header">
        <h2>Climate Vulnerability Indicator Suite (CVIS)</h2>
        <p>Supplemental Conservation Unit Reports for publication.</p>
      </div>

      <div class="about-section">
        <h3>About the Project</h3>
        <p>This supplement explores climate vulnerability indicator data for Conservation Units (CUs) within the Fraser River Basin. Click on any of the CUs in the left sidebar to view its individual profile, maps, life-stage timing, and indicator summary tables.</p>
        
        <p>Each individual report contains the following sections:</p>
        <ul>
          <li><strong>Overview:</strong> A structured summary of all demographic, freshwater, genetics, and migration vulnerability scores.</li>
          <li><strong>Demographics:</strong> Recent conservation status and spawner abundance trends.</li>
          <li><strong>Timing:</strong> Life-history stage timing throughout the year.</li>
          <li><strong>Spawning & Rearing:</strong> Freshwater stream access, temperature projections, streamflow changes, and cumulative threat maps.</li>
          <li><strong>Migration:</strong> Channel width, migratory distance, and daily stream temperatures during upstream migration.</li>
        </ul>
      </div>

      <div class="about-section">
        <h3>Analysis Default Parameters</h3>
        <ul>
          <li><strong>RCP 4.5:</strong> Representative Concentration Pathway (Scenario)</li>
          <li><strong>Time Period:</strong> Mid-Century (2041-2060)</li>
        </ul>
      </div>

      <div class="disclaimer-box">
        <h4>&#9888; Draft Data Disclaimer</h4>
        <p><strong>DRAFT - Not for further distribution without permission of the authors</strong></p>
        <p>This data and analysis are preliminary and subject to change. The indicators presented here are under active development and have not been peer-reviewed. Results should be interpreted with caution and are intended for exploratory analysis only.</p>
      </div>
    </div>

    <!-- Report Frame -->
    <iframe id="report-iframe"></iframe>
  </div>

  <script>
    /* CU_DATA_JSON_PLACEHOLDER */

    const listContainer = document.getElementById('cu-list-container');
    const iframe = document.getElementById('report-iframe');
    const aboutLanding = document.getElementById('about-landing');

    // Group CUs by species
    const speciesGroups = {};
    cuData.forEach(cu => {
      if (!speciesGroups[cu.species]) {
        speciesGroups[cu.species] = [];
      }
      speciesGroups[cu.species].push(cu);
    });

    // Render list
    function renderList() {
      listContainer.innerHTML = '';
      for (const [species, cus] of Object.entries(speciesGroups)) {
        const groupDiv = document.createElement('div');
        groupDiv.className = 'species-group';
        
        const titleDiv = document.createElement('div');
        titleDiv.className = 'species-title';
        titleDiv.innerText = species;
        groupDiv.appendChild(titleDiv);

        cus.forEach(cu => {
          const itemA = document.createElement('a');
          itemA.className = 'cu-item';
          if (!cu.hasReport) {
            itemA.classList.add('disabled');
          }
          itemA.id = 'cu-' + cu.code;
          itemA.innerText = cu.code + ' - ' + cu.name;
          itemA.onclick = () => selectCU(cu);
          groupDiv.appendChild(itemA);
        });

        listContainer.appendChild(groupDiv);
      }
    }

    function selectCU(cu) {
      if (!cu.hasReport) return;
      
      // Update active styling
      document.querySelectorAll('.cu-item').forEach(el => el.classList.remove('active'));
      document.getElementById('cu-' + cu.code).classList.add('active');

      // Decode and inject report
      try {
        const decodedHtml = decodeURIComponent(escape(atob(cu.base64)));
        aboutLanding.style.display = 'none';
        iframe.style.display = 'block';
        iframe.srcdoc = decodedHtml;
      } catch (e) {
        console.error("Failed to decode HTML for CU:", cu.code, e);
      }
    }

    function showAbout() {
      document.querySelectorAll('.cu-item').forEach(el => el.classList.remove('active'));
      iframe.style.display = 'none';
      aboutLanding.style.display = 'flex';
      iframe.srcdoc = '';
    }

    function filterCUs() {
      const query = document.getElementById('search-box').value.toLowerCase();
      document.querySelectorAll('.cu-item').forEach(item => {
        const text = item.innerText.toLowerCase();
        if (text.includes(query)) {
          item.style.display = 'block';
        } else {
          item.style.display = 'none';
        }
      });
      
      // Hide species titles if all their items are hidden
      document.querySelectorAll('.species-group').forEach(group => {
        const items = group.querySelectorAll('.cu-item');
        let visibleCount = 0;
        items.forEach(item => {
          if (item.style.display !== 'none') visibleCount++;
        });
        const title = group.querySelector('.species-title');
        if (visibleCount === 0) {
          title.style.display = 'none';
        } else {
          title.style.display = 'block';
        }
      });
    }

    // Initialize list
    renderList();
  </script>
</body>
</html>
)---"

# Inject JSON array into HTML template
master_html <- sub("/* CU_DATA_JSON_PLACEHOLDER */", cu_json_array, html_template, fixed = TRUE)

# Write output file
output_file <- file.path(paths$reports, "CVIS_CU_Supplemental_Report.html")
writeLines(master_html, output_file)
cat("Stitched HTML dashboard written successfully to:", output_file, "\n")
