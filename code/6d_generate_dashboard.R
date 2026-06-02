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
  <title>Climate Vulnerability Indicators for Salmon (CVIS) Supplement - CU Data Reports</title>
  <link href="https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap" rel="stylesheet">
  <style>
    :root {
      --primary: #334155;
      --primary-light: #475569;
      --primary-hover: #475569;
      --bg: #F8FAFC;
      --sidebar-bg: #0F172A;
      --sidebar-hover: #1E293B;
      --text: #0F172A;
      --text-muted: #475569;
      --border: #E2E8F0;
      --active-bg: #334155;
      --active-text: #FFFFFF;
    }
    
    * {
      box-sizing: border-box;
      margin: 0;
      padding: 0;
    }
    
    body {
      font-family: 'Inter', -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif;
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
      border-bottom: 1px solid #1E293B;
    }
    
    .sidebar-header h1 {
      font-family: 'Inter', sans-serif;
      font-size: 1.15rem;
      font-weight: 700;
      color: #FFFFFF;
      margin-bottom: 5px;
    }
    
    .sidebar-header p {
      font-size: 0.7rem;
      color: #94A3B8;
      text-transform: uppercase;
      letter-spacing: 0.05em;
    }
    
    .btn-home {
      display: block;
      width: 100%;
      text-align: left;
      background: none;
      border: none;
      color: #94A3B8;
      font-size: 0.8rem;
      font-weight: 600;
      margin-top: 12px;
      cursor: pointer;
      transition: color 0.2s;
    }
    
    .btn-home:hover {
      color: #FFFFFF;
    }
    
    .search-container {
      padding: 15px 20px;
      border-bottom: 1px solid #1E293B;
    }
    
    .search-input {
      width: 100%;
      padding: 8px 12px;
      background-color: #1E293B;
      border: 1px solid #334155;
      border-radius: 4px;
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
      font-family: 'Inter', sans-serif;
      font-size: 0.75rem;
      font-weight: 700;
      text-transform: uppercase;
      letter-spacing: 0.05em;
      color: #64748B;
      margin-bottom: 8px;
      padding-left: 5px;
    }
    
    .cu-item {
      display: block;
      padding: 6px 10px;
      border-radius: 4px;
      color: #CBD5E1;
      text-decoration: none;
      font-size: 0.8rem;
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
      font-family: 'Inter', sans-serif;
      font-size: 1.75rem;
      font-weight: 700;
      color: var(--primary);
      margin-bottom: 10px;
      border-bottom: 2px solid var(--border);
      padding-bottom: 15px;
    }
    
    .about-header p {
      font-size: 1rem;
      color: var(--text-muted);
      line-height: 1.6;
    }
    
    .about-section {
      background: #FFFFFF;
      padding: 25px;
      border-radius: 4px;
      border: 1px solid var(--border);
      box-shadow: 0 1px 3px rgba(0,0,0,0.02);
    }
    
    .about-section h3 {
      font-family: 'Inter', sans-serif;
      font-size: 1.15rem;
      font-weight: 700;
      color: var(--primary);
      margin-bottom: 15px;
    }
    
    .about-section p, .about-section li {
      font-size: 0.9rem;
      line-height: 1.6;
      margin-bottom: 10px;
    }
    
    .about-section ul {
      padding-left: 20px;
      margin-bottom: 15px;
    }
    
    .disclaimer-box {
      background-color: #FEF2F2;
      border: 1px solid #FCA5A5;
      padding: 20px;
      border-radius: 4px;
      color: #991B1B;
    }
    
    .disclaimer-box h4 {
      font-family: 'Inter', sans-serif;
      font-size: 1rem;
      font-weight: 700;
      margin-bottom: 8px;
      display: flex;
      align-items: center;
      gap: 6px;
    }
    
    .disclaimer-box p {
      font-size: 0.85rem;
      line-height: 1.6;
    }
  </style>
</head>
<body>
 
  <!-- Sidebar -->
  <div class="sidebar">
    <div class="sidebar-header">
      <h1>CU Reports</h1>
      <p>Climate Vulnerability Indicators for Salmon</p>
      <button class="btn-home" onclick="showAbout()">Home / About Project</button>
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
        <h2>Climate Vulnerability Indicators for Salmon (CVIS)</h2>
        <p>Supplement - Conservation Unit Reports</p>
      </div>
 
      <div class="about-section">
        <h3>About the Project</h3>
        <p>This supplemental report presents individual Conservation Unit (CU) profiles and indicator data for the Climate Vulnerability Indicators for Salmon (CVIS) framework. Click on any of the CUs in the left sidebar to view its individual profile, maps, life-stage timing, and indicator summary tables.</p>
        
        <p>Each individual report contains the following sections:</p>
        <ul>
          <li><strong>Overview:</strong> A composite summary of Overall Vulnerability and category vulnerability scores compared across all CUs, alongside a complete baseline raw and standardized indicator table and locator maps.</li>
          <li><strong>Demographics:</strong> Recent Wild Salmon Policy (WSP) status assessments and generational spawner abundance trends.</li>
          <li><strong>Timing:</strong> Timeline of annual life-history stage schedules (run timing, migration, and peak spawning).</li>
          <li><strong>Spawning & Rearing:</strong> Freshwater rearing habitat stream networks, stream temperature projections, warming rates, summer/winter flow alterations, and habitat niche suitability.</li>
          <li><strong>Migration:</strong> Upstream adult migration route characteristics, including migration distance and daily mainstem stream temperatures synced to CU migration windows.</li>
          <li><strong>Marine:</strong> Nearshore marine climate exposure (SST projections, decadal warming rates, and cumulative human impacts) across Marine Adaptive Zones (MAZs).</li>
          <li><strong>Sensitivity Analysis:</strong> Variations in overall scoring and standardized risk scores across different climate models (GCMs), downscaling methods, and aggregation algorithms.</li>
        </ul>
      </div>
 
      <div class="about-section">
        <h3>Analysis Assumptions</h3>
        <ul>
          <li><strong>Climate Scenario:</strong> Baseline vulnerability scores evaluate the RCP 4.5 emission pathway.</li>
          <li><strong>Time Period:</strong> Projections evaluate Mid-Century (2041-2060) changes.</li>
        </ul>
      </div>
 
      <div class="disclaimer-box">
        <h4>Draft Data Disclaimer</h4>
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
        let decodedHtml = decodeURIComponent(escape(atob(cu.base64)));
        // Inject <base href="CU_reports/"> right after <head> so relative links to shared libs/ and unique files/ resolve correctly
        decodedHtml = decodedHtml.replace('<head>', '<head><base href="CU_reports/">');
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
