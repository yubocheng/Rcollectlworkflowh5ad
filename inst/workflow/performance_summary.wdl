version 1.0

task Rcollectl_h5ad_summary {
    input {
        Boolean knitr_eval
        String fileId
        String sample
        Int mem_gb = 120
    }

    command {
        /tmp/run_Rcollectl.R ${knitr_eval} ${fileId} ${sample} ${mem_gb}
    }

    output {
        File modelGeneVarByPoisson_html = "modelGeneVarByPoisson.html"
        Array[File] Rcollectl_result = glob("*.tab.gz")
    }

    runtime {
        docker: "ycheng2022/bioconductor_docker_workflow_h5ad_summary:devel"
        memory: mem_gb + "GB"
        cpu: 3
    }
}

workflow RcollectlWorkflowSummary {
    meta {
        description: "Provide summary of a single-cell analysis Workflow computing resources usage tracking with package Rcollectl"
    }
    
    input {
        Boolean knitr_eval
        String fileId
        String sample
        Int mem_gb = 240
    }

    call Rcollectl_h5ad_summary {
        input: 
        knitr_eval = knitr_eval, 
        fileId = fileId, 
        sample = sample,
        mem_gb = mem_gb
    }
    
    output {
    	File modelGeneVarByPoisson_html = Rcollectl_h5ad_summary.modelGeneVarByPoisson_html
    	Array[File] Rcollectl_result = Rcollectl_h5ad_summary.Rcollectl_result
    }
}
