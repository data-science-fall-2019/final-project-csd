1. I see only one Rmd file. Make sure you have much more before the final submission. 

2. Each Rmd file should be small and contain a self-contained analysis. Don't do everything in one Rmd file. E.g. the "sam's part" and "ai's part" should be different Rmd's if they aren't connected. Ideally each file would be connected with a type of analysis and not with the person who performs the analysis.

3. The barplot doesn't seem meaningful for Type vs retention. I think this just sums up the proportions from the retention rates for each type? Don't include things that aren't meaningful.

4. Any idea why there are so many rounded retention rates (0, 0.5, 1)?

5. Make sure you always provide comments on a plot. Don't just present a plot --- people who didn't make the plot will have trouble understanding what it says.

6. In Rmd files, you generally don't need code comments since those should just be plain text outside of the code chunks. Please move comments to outside code chunks.

7. The linear regression for Out of state tuition on average faculty salary doesn't seem appropriate. There appears to be two clouds of points --- probably one of private schools and one of public. So this would indicate separate linear models for those two groups.

8. There should only be one shiny app. Not one per person. Make sure you collaborate on the shiny app.

9. Why is there a "variational genotyping" README in the data folder. Get rid of unused files.

10. You should generally never zip files for GitHub. GitHub has its own compression system. And it adds a layer of complexity for users to navigate a zipped file. Use LFS from GitHub. I believe I set this up for you guys, but those files seem to have been deleted.

11. Make sure to update your README before submission. Make it an executive summary of your project --- meant for folks who have no knowledge of the project. Here is an example from my work: <https://github.com/dcgerard/updog>

12. I like Ai's idea for the Shiny app the best. The other ideas seem mostly like copies of what we did in class.
