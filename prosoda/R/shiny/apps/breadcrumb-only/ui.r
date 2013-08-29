shinyUI(bootstrapPage(

	div(class = "container",
		div(class = "row",
			breadcrumbOutput("quantarchBreadcrumb" )),
		div(class = "row",
			div(class = "span12",
			    selectInput("currentapp", "Select current app:",
			                c("Projects" = "projects",
			                  "Dashboard" = "dashboard",
			                  "Commit Information" = "commit.info",
			                  "Commit Structure" = "commit.structure",
			                  "Contributions overview" = "contributions",
			                  "Contributors" = "contributors",
			                  "Activity punch cards" = "punchcard",
			                  "ML activity punch cards" = "punchcard_ml",
			                  "Inter-Release Distance" = "release_distance",
			                  "Mailing list activity" = "timeseries",
			                  "Collaboration clusters" = "vis.clusters")
                        )
          )
			)
		)
	))
